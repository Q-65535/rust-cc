use std::{io::{self, Write}, collections::VecDeque, process::exit, fs::File, sync::Mutex};
use std::cell::Cell;
use colored::*;
use crate::ir::ExprType::{self, *};
use crate::ir::StmtType::{self, *};
use crate::ir::OP::{self, *};
use crate::ir::Data_Directive::{self, *};
use crate::Declaration;
use crate::Function;
use crate::Obj;
use crate::Type::{self, *};
use crate::SRC;
use crate::INPUT_PATH;
use crate::ir::{self, *};
use crate::analyze::{self, *};
use crate::common::{self, *};

/*----------------------abbriviation----------------------
    gp: general purpose
    fp: floating point
----------------------abbriviation----------------------*/


// Where generated assembly is written.
enum Output {
    Stdout,
    File(File),
}

static OUTPUT: Mutex<Output> = Mutex::new(Output::Stdout);

// `-o <path>` selects the file; a missing path or "-" means stdout.
pub fn set_output(opt_o: &Option<String>) {
    let mut out = OUTPUT.lock().unwrap();
    *out = match opt_o {
        Some(path) if path != "-" => Output::File(File::create(path).unwrap_or_else(|err| {
            eprintln!("cannot open output file: {}: {}", path, err);
            exit(1);
        })),
        _ => Output::Stdout,
    };
}

macro_rules! emit {
    () => {{
        match &mut *OUTPUT.lock().unwrap() {
            Output::Stdout => { let _ = writeln!(io::stdout()); }
            Output::File(f) => { let _ = writeln!(f); }
        }
    }};
    ($($arg:tt)*) => {{
        match &mut *OUTPUT.lock().unwrap() {
            Output::Stdout => { let _ = writeln!(io::stdout(), $($arg)*); }
            Output::File(f) => { let _ = writeln!(f, $($arg)*); }
        }
    }};
}

macro_rules! emit_raw {
    ($($arg:tt)*) => {{
        match &mut *OUTPUT.lock().unwrap() {
            Output::Stdout => { let _ = write!(io::stdout(), $($arg)*); }
            Output::File(f) => { let _ = write!(f, $($arg)*); }
        }
    }};
}

pub struct Generator {
    cur_function_stack_size: usize,
    cur_function_name: String,
    argregs64: Vec<&'static str>,
    argregs32: Vec<&'static str>,
    argregs16: Vec<&'static str>,
    argregs8: Vec<&'static str>,
    // Number of 8-byte values currently pushed on the stack within the
    // function being emitted. Used to keep RSP 16-byte aligned at `call`.
    depth: usize,
    jump_label_count: usize,
}

impl Generator {
    pub fn new() -> Self {
        let argregs64 = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8",  "%r9" ];
        let argregs32 = vec!["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
        let argregs16 = vec![ "%di",  "%si",  "%dx",  "%cx", "%r8w", "%r9w"];
        let argregs8  = vec![ "%dil", "%sil", "%dl",  "%cl", "%r8b", "%r9b"];
        Self {
            jump_label_count: 0,
            cur_function_stack_size: 0,
            cur_function_name: "".to_string(),
            argregs64, argregs32, argregs16, argregs8,
            depth: 0,
        }
    }

    pub fn gen_code(&mut self, program: AnalyzedProgram) {
        emit!(".file 1 \"{}\"", INPUT_PATH.lock().unwrap());
        for global_decl in &program.global_data_decls {
            if global_decl.obj.is_extern {
                continue;
            }
            emit!("");
            if global_decl.obj.is_static {
                emit!("  .local {}", global_decl.obj.name);
            } else {
                emit!("  .globl {}", global_decl.obj.name);
            }
            emit!("  .align {}", global_decl.obj.align);
            if let Some(data_directives) = &global_decl.init_data {
                emit!("  .data");
                emit!("{}:", global_decl.obj.name);
                for data_directive in data_directives {
                    match data_directive {
                        ASM_Byte(num) => emit!("  .byte {}", num),
                        ASM_Word(num) => emit!("  .word {}", num),
                        ASM_Long(num) => emit!("  .long {}", num),
                        ASM_Quad(num) => emit!("  .quad {}", num),
                        ASM_Labeled_Quad(label, num) => emit!("  .quad {}{:+}",label, num),
                        ASM_String(s) => emit!("  .string \"{}\"",s),
                    }
                }
            } else {
                emit!("  .bss");
                emit!("{}:", global_decl.obj.name);
                emit!("  .zero {}", sizeof(&global_decl.obj.ty));
            }
        }
        for fun in program.afuns {
            self.cur_function_stack_size = fun.stack_size;
            self.cur_function_name = fun.name.clone();
            self.depth = 0;
            self.fun_gen(fun);
        }
        emit!("  .section .note.GNU-stack,\"\",@progbits");
    }

    pub fn fun_gen(&mut self, fun: ir::Function) {
        let stack_size = fun.stack_size;
        let aligned_stack_size = align_to(stack_size, 16);
        // prologue
        emit!();
        if fun.is_static {
            emit!("  .local {}", fun.name);
        } else {
            emit!("  .globl {}", fun.name);
        }
        emit!("  .text");
        emit!("{}:", fun.name);
        emit!("  push %rbp");
        emit!("  mov %rsp, %rbp");
        emit!("  sub ${}, %rsp", aligned_stack_size);
        emit!();

        // Save arg registers if function is variadic
        if let Some(var_area) = fun.var_area {
            let mut gp = 0;
            let mut fp = 0;
            for param in &fun.params {
                if param.ty.is_fp() {
                    fp += 1;
                } else {
                    gp += 1;
                }
            }
            let var_area_offset = self.get_absolute_offset(&var_area);
            /*
                    the struct layout is:
                    typedef struct {
                      int gp_offset;
                      int fp_offset;
                      void *overflow_arg_area;
                      void *reg_save_area;
                    } __va_elem;
            */
            // gp_offset is the byte offset into reg_save_area for
            // the first integer arg in the variadic args.
            emit!("  movl ${}, {}(%rbp)", gp * 8, var_area_offset);
            // fp_offset is the byte offset into reg_save_area for
            // the first floating point arg in the variadic args.
            // Why we here add extra 48 to offset? Because the first 48 bytes in reg_save_area
            // is occupied by 6 gp register values and each gp register is 8 bytes (6x8=48).
            emit!("  movl ${}, {}(%rbp)", fp * 8 + 48, var_area_offset + 4);

            // set reg_save_area
            emit!("  movq %rbp, {}(%rbp)", var_area_offset + 16);
            emit!("  addq ${}, {}(%rbp)", var_area_offset + 24, var_area_offset + 16);

            // __reg_save_area__
            emit!("  movq %rdi, {}(%rbp)", var_area_offset + 24);
            emit!("  movq %rsi, {}(%rbp)", var_area_offset + 32);
            emit!("  movq %rdx, {}(%rbp)", var_area_offset + 40);
            emit!("  movq %rcx, {}(%rbp)", var_area_offset + 48);
            emit!("  movq %r8, {}(%rbp)", var_area_offset + 56);
            emit!("  movq %r9, {}(%rbp)", var_area_offset + 64);
            emit!("  movsd %xmm0, {}(%rbp)", var_area_offset + 72);
            emit!("  movsd %xmm1, {}(%rbp)", var_area_offset + 80);
            emit!("  movsd %xmm2, {}(%rbp)", var_area_offset + 88);
            emit!("  movsd %xmm3, {}(%rbp)", var_area_offset + 96);
            emit!("  movsd %xmm4, {}(%rbp)", var_area_offset + 104);
            emit!("  movsd %xmm5, {}(%rbp)", var_area_offset + 112);
            emit!("  movsd %xmm6, {}(%rbp)", var_area_offset + 120);
            emit!("  movsd %xmm7, {}(%rbp)", var_area_offset + 128);
        }

        let mut fp_reg_index = 0;
        let mut gp_reg_index = 0;
        for param in fun.params {
            let concrete_param_offset = self.get_absolute_offset(&param);
            if param.ty.is_fp() {
                if param.ty == Float {
                    emit!("  movss %xmm{}, {}(%rbp)\n", fp_reg_index,  concrete_param_offset);
                } else if param.ty == Double {
                    emit!("  movsd %xmm{}, {}(%rbp)\n", fp_reg_index,  concrete_param_offset);
                }
                fp_reg_index += 1;
            } else {
                match sizeof(&param.ty) {
                    1 => emit!("  mov {}, {}(%rbp)\n", self.argregs8[gp_reg_index], concrete_param_offset),
                    2 => emit!("  mov {}, {}(%rbp)\n", self.argregs16[gp_reg_index], concrete_param_offset),
                    4 => emit!("  mov {}, {}(%rbp)\n", self.argregs32[gp_reg_index], concrete_param_offset),
                    _ => emit!("  mov {}, {}(%rbp)\n", self.argregs64[gp_reg_index], concrete_param_offset),
                }
                gp_reg_index += 1;
            }
        }
        self.block_gen(&fun.stmts);

        // end
        emit!(".L.return.{}:",fun.name);
        emit!("  mov %rbp, %rsp");
        emit!("  pop %rbp");
        emit!("  ret");
    }

    fn block_gen(&mut self, stmts: &Vec<ir::StmtType>) {
        for stmt in stmts {
            emit!(";;;;;;;;;;;;;;;;;;;;;;;;");
            self.stmt_gen(stmt);
            emit_raw!(";;;;;;;;;;;;;;;;;;;;;;;;");
        }
        emit!();
    }

    fn stmt_gen(&mut self, stmt: &ir::StmtType) {
        match stmt {
            ir::StmtType::Ex(expr) => self.expr_gen(&expr),
            ir::StmtType::Return(expr) => {
                if let Some(expr) = expr {
                    self.expr_gen(expr);
                }
                emit!("  jmp .L.return.{}\n", self.cur_function_name);
            }
            ir::StmtType::Block(item) =>self.block_gen(item),
            ir::StmtType::If{cond, then, otherwise} => self.if_gen(cond, then, otherwise),
            ir::StmtType::For{init, cond, inc, then, break_pos_label, continue_pos_label} => self.for_gen(init, cond, inc, then, break_pos_label, continue_pos_label),
            ir::StmtType::Do_While{then, cond, break_pos_label, continue_pos_label} => {
                let c = self.next_jump_label_count();
                emit!(".L.begin.{}:", c);
                self.stmt_gen(&then);
                emit!("{}:", continue_pos_label);
                self.expr_gen(cond);
                cmp_zero(&cond.ty);
                // emit!("  cmp $0, %rax");
                emit!("  jne .L.begin.{}", c);
                emit!("{}:", break_pos_label);
            }
            ir::StmtType::Goto(label) => emit!("  jmp {}", label),
            ir::StmtType::LabeledStmt(label, stmt) => {
                emit!("{}:", label);
                self.stmt_gen(stmt);
            }
            ir::StmtType::Switch{switch_case_info, body, break_pos_label} => self.switch_gen(switch_case_info, body, break_pos_label),
            ir::StmtType::CaseStmt{unique_label, stmt} => {
                emit!("{}:", unique_label);
                self.stmt_gen(stmt);
            }
        }
    }

    fn switch_gen(&mut self, switch_case_info: &Switch_Case, body: &StmtType, end_label: &str) {
        
        self.expr_gen(&switch_case_info.target_expr);
        let reg = if sizeof(&switch_case_info.target_expr.ty) == 8 {
            "%rax".to_string()
        } else {
            "%eax".to_string()
        };
        for case in &switch_case_info.cases {
            emit!("  cmp ${}, {}", case.cond_value, reg);
            emit!("  je {}", case.unique_label);
        }
        if let Some(d) = &switch_case_info.default_label {
            emit!("  jmp {}", d);
        }
        emit!("  jmp {}", end_label);

        self.stmt_gen(body);
        
        emit!("{}:", end_label);
    }

    fn if_gen(&mut self, cond: &ir::Expr, then: &ir::StmtType, otherwise: &Option<Box<ir::StmtType>>) {
        let c = self.next_jump_label_count();
        self.expr_gen(&cond);

        cmp_zero(&cond.ty);
        // emit!("  cmp $0, %rax");
        emit!("  je  .L.else.{}", c);
        self.stmt_gen(&then);
        emit!("  jmp .L.end.{}", c);
        emit!(".L.else.{}:", c);
        if let Some(els) = otherwise {
            self.stmt_gen(&els);
        }
        emit!(".L.end.{}:", c);
    }

    fn for_gen(&mut self, init: &Vec<StmtType>, cond: &Option<ir::Expr>, inc: &Option<ir::Expr>, then: &Box<ir::StmtType>, end_label: &str, continue_point_label: &str) {
        let c = self.next_jump_label_count();
        for init_stmt in init {
            self.stmt_gen(init_stmt);
        }
        emit!(".L.begin.{}:", c);
        if let Some(expr) = cond {
            self.expr_gen(expr);
            cmp_zero(&expr.ty);
            // emit!("  cmp $0, %rax");
            emit!("  je  {}", end_label);
        }
        self.stmt_gen(&then);
        emit!("{}:", continue_point_label);
        if let Some(expr) = inc {
            self.expr_gen(expr);
        }
        emit!("  jmp .L.begin.{}", c);
        emit!("{}:", end_label);
    }

    fn expr_gen(&mut self, expr: &ir::Expr) {
        //emit!("  .loc 1 {}", expr.span.get_start_line());
        let content = &expr.content;
        match content {
            Integer(n) => emit!("  mov ${}, %rax", n),
            ExprType::Float_Const(f) => {
                emit!("  mov ${}, %eax  # float {}", f.to_bits(), f);
                emit!("  movq %rax, %xmm0");
            }
            ExprType::Double_Const(f) => {
                emit!("  mov ${}, %rax  # double {}", f.to_bits(), f);
                emit!("  movq %rax, %xmm0");
            }
            CommaExpression(lhs, rhs) => {
                self.expr_gen(lhs);
                self.expr_gen(rhs);
            }
            // Although logic operators belong to binary operators,
            // they can be handeld in a unified way for integer and flonum.
            // Other binary operators are handled separatly for integer and flonum.
            Binary(lhs, rhs, op) if op.is_logic() => {
                match op {
                    LOGAND => {
                        let c = self.next_jump_label_count();
                        self.expr_gen(lhs);
                        cmp_zero(&lhs.ty);
                        emit!("  je .L.false.{}", c);
                        self.expr_gen(rhs);
                        cmp_zero(&rhs.ty);
                        emit!("  je .L.false.{}", c);
                        emit!("  mov $1, %rax");
                        emit!("  jmp .L.end.{}", c);
                        emit!(".L.false.{}:", c);
                        emit!("  mov $0, %rax");
                        emit!(".L.end.{}:", c);
                    }
                    LOGOR => {
                        let c = self.next_jump_label_count();
                        self.expr_gen(lhs);
                        cmp_zero(&lhs.ty);
                        emit!("  jne .L.true.{}", c);
                        self.expr_gen(rhs);
                        cmp_zero(&rhs.ty);
                        emit!("  jne .L.true.{}", c);
                        emit!("  mov $0, %rax");
                        emit!("  jmp .L.end.{}", c);
                        emit!(".L.true.{}:", c);
                        emit!("  mov $1, %rax");
                        emit!(".L.end.{}:", c);
                    }
                    _ => unreachable!(),
                }
            }
            Binary(lhs, rhs, op) => {
                if matches!(lhs.ty, Float | Double) {
                    self.expr_gen(rhs);
                    self.push_float("%xmm0");
                    self.expr_gen(lhs);
                    self.pop_float(1);

                    let sz = if lhs.ty == Float {"ss"} else {"sd"};
                    match op {
                        op if op.is_compare() => {
                            emit!("ucomi{} %xmm0, %xmm1", sz);
                            match op {
                                Eq => {
                                    emit!("  sete %al");
                                    emit!("  setnp %dl");
                                    emit!("  and %dl, %al");
                                }
                                Neq => {
                                    emit!("  setne %al");
                                    emit!("  setp %dl");
                                    emit!("  or %dl, %al");
                                }
                                LT => {
                                    emit!("  seta %al");
                                }
                                LE => {
                                    emit!("  setae %al");
                                }
                                _ => unreachable!(),
                            }
                            emit!("and $1, %al");
                            emit!("movzb %al, %rax");
                        }
                        Plus =>  emit!("  add{} %xmm1, %xmm0", sz),
                        Minus => emit!("  sub{} %xmm1, %xmm0", sz),
                        Mul => emit!("  mul{} %xmm1, %xmm0", sz),
                        Div => emit!("  div{} %xmm1, %xmm0", sz),
                        _ => {
                            eprintln!("Compiler bug: Unable to handle {:?} operator for flonum.", op);
                            exit(1);
                        }
                    }
                } else {
                    self.expr_gen(rhs);
                    self.push("%rax");
                    self.expr_gen(lhs);
                    self.pop("%rdi");
                    let (ax, di, dx) = if matches!(lhs.ty, Long | ULong | Pointer_To(..) | ArrayOf(..)) {
                        ("%rax", "%rdi", "%rdx")
                    } else {
                        ("%eax", "%edi", "%edx")
                    };
                    match op {
                        Plus =>  emit!("  add {}, {}", di, ax),
                        Minus => emit!("  sub {}, {}", di, ax),
                        Mul =>   emit!("  imul {}, {}", di, ax),
                        Div | Modulus => {
                            if expr.ty.is_unsigned() {
                              emit!("  mov $0, {}", dx);
                              emit!("  div     {}", di);
                            } else {
                                if sizeof(&lhs.ty) == 8 {
                                    emit!("  cqo");
                                } else {
                                    emit!("  cdq");
                                }
                                emit!("  idiv {}", di);
                            }
                            if *op == Modulus {
                                emit!("  mov %rdx, %rax");
                            }
                        },
                        BitAnd => emit!("  and {}, {}", di, ax),
                        BitOR => emit!("  or {}, {}", di, ax),
                        BitXOR => emit!("  xor {}, {}", di, ax),
                        SHL => {
                            emit!("  mov %rdi, %rcx");
                            emit!("  shl %cl, {}", ax);
                        }
                        SHR => {
                            emit!("  mov %rdi, %rcx");
                            if lhs.ty.is_unsigned() {
                                emit!("  shr %cl, {}", ax);
                            } else {
                                emit!("  sar %cl, {}", ax);
                            }
                        }
                        op if op.is_compare() => {
                            emit!("  cmp {}, {}", di, ax);
                            match op {
                                Eq => emit!("  sete %al"),
                                Neq => emit!("  setne %al"),
                                LT => {
                                    if lhs.ty.is_unsigned() {
                                        emit!("  setb %al");
                                    } else {
                                        emit!("  setl %al");
                                    }
                                }
                                LE => {
                                    if lhs.ty.is_unsigned() {
                                        emit!("  setbe %al");
                                    } else {
                                        emit!("  setle %al");
                                    }
                                }
                                _ => unreachable!(),
                            }
                            emit!("  movzb %al, %rax");
                        },
                        _ => eprintln!("gen_code error: not support binary expr {:?}", content),
                    }
                }
            }
            Conditional{cond, then, otherwise} => {
                let c = self.next_jump_label_count();
                self.expr_gen(cond);
                cmp_zero(&cond.ty);
                // emit!("  cmp $0, %rax");
                emit!("  je .L.else.{}", c);
                self.expr_gen(then);
                emit!("  jmp .L.end.{}", c);
                emit!(".L.else.{}:", c);
                self.expr_gen(otherwise);
                emit!(".L.end.{}:", c);
                return;
            }
            Assign(var, val) => {
                self.gen_addr(var);
                self.push("%rax");
                self.expr_gen(val);
                self.store_according_to_type(&var.ty);
            }
            Neg(expr) => {
                self.expr_gen(expr);
                match &expr.ty {
                    Float => {
                        emit!("  mov $1, %rax");
                        emit!("  shl $31, %rax");
                        emit!("  movq %rax, %xmm1");
                        emit!("  xorps %xmm1, %xmm0");
                    }
                    Double => {
                        emit!("  mov $1, %rax");
                        emit!("  shl $63, %rax");
                        emit!("  movq %rax, %xmm1");
                        emit!("  xorps %xmm1, %xmm0");
                    }
                    _ => emit!("  neg %rax"),
                }
            }
            Not(expr) => {
                self.expr_gen(expr);
                cmp_zero(&expr.ty);
                // emit!("  cmp $0, %rax");
                emit!("  sete %al");
                emit!("  movzx %al, %rax");
            }
            BitNot(expr) => {
                self.expr_gen(expr);
                emit!("  not %rax");
            }
            Deref(inner_expr) => {
                self.expr_gen(inner_expr);
                load_according_to_type(&expr.ty);
            }
            AddrOf(expr) => self.gen_addr(expr),
            Object(obj) => {
                self.gen_addr(expr);
                load_according_to_type(&expr.ty);
            }
            RequestStructMember(st, offset) => {
                self.gen_addr(expr);
                load_according_to_type(&expr.ty);
            }
            Cast(inner_expr, ty) => {
                self.expr_gen(inner_expr);
                cast(&inner_expr.ty, ty);
            }
            CompLit(stmts, expr) => {
                self.block_gen(stmts);
                self.expr_gen(expr);
            }
            FunCall(func_ref, args) => {
                // Generate all the args and put them temorary on stack.
                for arg in args.into_iter().rev() {
                    self.expr_gen(arg);
                    if arg.is_fp() {
                        self.push_float("%xmm0");
                    } else {
                        self.push("%rax");
                    }
                }

                self.expr_gen(func_ref);
                // Then put those args on the stack into designated registers.
                let mut fp_reg_index = 0;
                let mut gp_reg_index = 0;
                for arg in args {
                    if arg.is_fp() {
                        self.pop_float(fp_reg_index);
                        fp_reg_index += 1;
                    } else {
                        self.pop(self.argregs64[gp_reg_index]);
                        gp_reg_index += 1;
                    }
                }
                // The x86-64 ABI requires RSP to be 16-byte aligned
                // at the point of a `call`. If an odd number of
                // 8-byte values are live on the stack, realign first.
                let needs_align = self.depth % 2 == 1;
                if needs_align {
                    emit!("  sub $8, %rsp");
                }
                let fp_args_count = match &func_ref.ty {
                    Func{is_variadic, ..} if *is_variadic => fp_reg_index,
                    _ => 0,
                };

                // In x64 ABI, When calling variadic function, %al indicates how many
                // floting point number are passed as arguments.
                // @Wrong?: Since we support function pointer, %rax register now
                // stores the callee address, it has nothing to do with floating pointer
                // arguments anymore!
                // emit!("  mov ${}, %rax", fp_args_count);
                // emit!("  call {}", obj.name);
                emit!("  call *%rax");
                if needs_align {
                    emit!("  add $8, %rsp");
                }
                // @Temporary: This is just for being compatible with chibicc's
                // test suits which is, weird. In its commit "Handle a function
                // returning bool, char or short", the return type of the 
                // functions declared in common is different to that in function.c
                // If they are the same, we don't need the following match cases.
                // You might be wondering why redeclaring a function with a different
                // return type doesn't cause a compile error? Well, that's because
                // the file "common" is not processed in our compiler, it is processed
                // after the compilation as specified in Makefile.
                match &expr.ty {
                    Bool  =>   emit!("  movzx %al, %eax"),
                    Char  =>   emit!("  movsbl %al, %eax"),
                    Short =>   emit!("  movswl %ax, %eax"),
                    UChar  =>  emit!("  movzbl %al, %eax"),
                    UShort =>  emit!("  movzwl %ax, %eax"),
                    _ => (),
                }
            }
            StmtExpr(stmts) => self.block_gen(stmts),
            _ => eprintln!("gen_code error: not support {:?}", content),
        }
    }

    fn push_float(&mut self, reg: &str) {
        emit!("  sub $8, %rsp");
        emit!("  movsd {}, (%rsp)", reg);
        self.depth += 1;
    }

    fn pop_float(&mut self, reg_index: u8) {
        emit!("  movsd (%rsp), %xmm{}", reg_index);
        emit!("  add $8, %rsp");
        self.depth -= 1;
    }


    fn next_jump_label_count(&mut self) -> usize {
        self.jump_label_count += 1;
        return self.jump_label_count;
    }

    fn gen_addr(&mut self, expr: &Expr) {
        match &expr.content {
            Object(obj) => {
                if !obj.is_global && !obj.is_extern { // local variable
                    let concrete_offset = self.get_absolute_offset(&obj);
                    emit!("  lea {}(%rbp), %rax", concrete_offset);
                    return;
                }
                if matches!(obj.ty, Func{..}) {
                    if obj.is_extern {
                        // Dynamic link situation:
                        emit!("  mov {}@GOTPCREL(%rip), %rax", obj.name);
                    } else {
                        // Static link situation:
                        emit!("  lea {}(%rip), %rax", obj.name);
                    }
                    return;
                }
                if obj.is_global || obj.is_extern { // other non-funtion symbols
                    emit!("  lea {}(%rip), %rax", obj.name);
                    return;
                }
            }
            Deref(expr) => {
                self.expr_gen(expr);
            },
            CommaExpression(lhs, rhs) => {
                self.expr_gen(lhs);
                self.gen_addr(rhs);
            },
            RequestStructMember(st, offset) => {
                self.gen_addr(st);
                emit!("  add ${}, %rax", offset);
            },
            CompLit(stmts, expr) => {
                self.block_gen(stmts);
                self.gen_addr(expr);
            }
            _ => {
                let err_msg = error_expr(expr, "codegen error: can't get addr of this expr");
                eprintln!("{}", err_msg);
                exit(1);
            },
        }
    }

    fn get_absolute_offset(&self, obj: &Obj) -> i64 {
        let stack_bottom_offset_to_rbp = -(self.cur_function_stack_size as i64);
        return stack_bottom_offset_to_rbp + (obj.offset as i64);
    }

    fn push(&mut self, reg: &str) {
        emit!("  push {}", reg);
        self.depth += 1;
    }

    fn pop(&mut self, reg: &str) {
        emit!("  pop {}", reg);
        self.depth -= 1;
    }

    fn store_according_to_type(&mut self, ty: &Type) {
        self.pop("%rdi");

        match ty {
            Struct(..) | Union(..) => {
                for i in 0..sizeof(ty) {
                    emit!("  mov {}(%rax), %r8b", i);
                    emit!("  mov %r8b, {}(%rdi)", i);
                }
            }
            Type::Float => emit!("  movss %xmm0, (%rdi)"),
            Type::Double => emit!("  movsd %xmm0, (%rdi)"),
            _ => {
                match sizeof(ty) {
                    1 => emit!("  mov  %al, (%rdi)"),
                    2 => emit!("  mov  %ax, (%rdi)"),
                    4 => emit!("  mov %eax, (%rdi)"),
                    _ => emit!("  mov %rax, (%rdi)"),
                }
            }
        }
    }

}

fn cast(from: &Type, to: &Type) {
    if to == &Void {return;}
    if (to == &Bool) {
        cmp_zero(from);
        emit!("  setne %al");
        emit!("  movzx %al, %eax");
        return;
    }

    let from_fundemental_type = get_fundemental_type(from);
    let to_fundemental_type   = get_fundemental_type(to);
    gen_cast_operation(from_fundemental_type, to_fundemental_type);
}

fn cmp_zero(ty: &Type) {
    match ty {
        Float => {
            emit!("  xorps %xmm1, %xmm1");
            emit!("  ucomiss %xmm1, %xmm0");
        }
        Double => {
            emit!("  xorpd %xmm1, %xmm1");
            emit!("  ucomisd %xmm1, %xmm0");
        }
        _ => {
            if ty.is_integer() && sizeof(ty) <= 4 {
                emit!("  cmp $0, %eax");
            } else {
                emit!("  cmp $0, %rax");
            }
        }
    }
}


// Every time we do casting, we first try to get the cast-from and cast-to
// fundemental type from their original C types, then generate different
// assembly code according to their fundemental types.
// Emm..., so this enum exists only for type casting I guess?
// Maybe it has other use cases in the future, I don't knowwwwwwww.
#[derive(Debug, Clone, PartialEq)]
pub enum Fundemental_Type {
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
}
use Fundemental_Type::*;


fn get_fundemental_type(ty: &Type) -> Fundemental_Type {
    match ty {
        // Bool is special. Because it is normalized to either 0 or 1
        // in 64-bit range in register, we can just use I64 without any problem.
        Bool   =>   I64,
        Char   =>   I8,
        Short  =>   I16,
        Int | Enum => I32,
        Long   =>   I64, 
        UChar  =>   U8,
        UShort =>   U16,
        UInt   =>   U32,
        ULong  =>   U64, 
        Float  =>   F32,
        Double =>   F64,
        Pointer_To(..) | ArrayOf(..) | Func{..} => U64,
        _ => {
            println!("cannot get the fundemental type of this type: {:?}", ty);
            exit(1);
        }
    }
}

fn gen_cast_operation(from: Fundemental_Type, to: Fundemental_Type) {
    let i32i8  = "  movsbl  %al, %eax";
    let i32u8  = "  movzbl  %al, %eax";
    let i32i16 = "  movswl  %ax, %eax";
    let i32u16 = "  movzwl  %ax, %eax";
    let i32f32 = "  cvtsi2ssl %eax, %xmm0";
    let i32i64 = "  movsxd %eax, %rax";
    let i32f64 = "  cvtsi2sdl %eax, %xmm0";

    let u32f32 = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
    let u32i64 = "  mov    %eax, %eax";
    let u32f64 = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";

    let i64f32 = "cvtsi2ssq %rax, %xmm0";
    let i64f64 = "cvtsi2sdq %rax, %xmm0";

    let u64f32 = "cvtsi2ssq %rax, %xmm0";
    let u64f64 =
      "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f;
      1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi;
      or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:";

    let f32i8 = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
    let f32u8 = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
    let f32i16 = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
    let f32u16 = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
    let f32i32 = "cvttss2sil %xmm0, %eax";
    let f32u32 = "cvttss2siq %xmm0, %rax";
    let f32i64 = "cvttss2siq %xmm0, %rax";
    let f32u64 = "cvttss2siq %xmm0, %rax";
    let f32f64 = "cvtss2sd %xmm0, %xmm0";

    let f64i8 = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
    let f64u8 = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
    let f64i16 = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
    let f64u16 = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
    let f64i32 = "cvttsd2sil %xmm0, %eax";
    let f64u32 = "cvttsd2siq %xmm0, %rax";
    let f64f32 = "cvtsd2ss %xmm0, %xmm0";
    let f64i64 = "cvttsd2siq %xmm0, %rax";
    let f64u64 = "cvttsd2siq %xmm0, %rax";

    fn direct_emit(ins: &str) { emit!("{}", ins); }

    match from {
        I8 => match to {
            I8  => (),
            I16 => (),
            I32 => (),
            I64 => direct_emit(i32i64),
            U8  => direct_emit(i32u8),
            U16 => direct_emit(i32u16),
            U32 => (),
            U64 => direct_emit(i32i64),
            F32 => direct_emit(i32f32),
            F64 => direct_emit(i32f64),
        }
        I16 => match to {
            I8  => direct_emit(i32i8),
            I16 => (),
            I32 => (),
            I64 => direct_emit(i32i64),
            U8  => direct_emit(i32u8),
            U16 => direct_emit(i32u16),
            U32 => (),
            U64 => direct_emit(i32i64),
            F32 => direct_emit(i32f32),
            F64 => direct_emit(i32f64),
        }
        I32 => match to {
            I8  => direct_emit(i32i8),
            I16 => direct_emit(i32i16),
            I32 => (),
            I64 => direct_emit(i32i64),
            U8  => direct_emit(i32u8),
            U16 => direct_emit(i32u16),
            U32 => (),
            U64 => direct_emit(i32i64),
            F32 => direct_emit(i32f32),
            F64 => direct_emit(i32f64),
        }
        I64 => match to {
            I8  => direct_emit(i32i8),
            I16 => direct_emit(i32i16),
            I32 => (),
            I64 => (),
            U8  => direct_emit(i32u8),
            U16 => direct_emit(i32u16),
            U32 => (),
            U64 => (),
            F32 => direct_emit(i64f32),
            F64 => direct_emit(i64f64),
        }
        U8 => match to {
            I8  => direct_emit(i32i8),
            I16 => (),
            I32 => (),
            I64 => direct_emit(i32i64),
            U8  => (),
            U16 => (),
            U32 => (),
            U64 => direct_emit(i32i64),
            F32 => direct_emit(i32f32),
            F64 => direct_emit(i32f64),
        }
        U16 => match to {
            I8  => direct_emit(i32i8),
            I16 => direct_emit(i32i16),
            I32 => (),
            I64 => direct_emit(i32i64),
            U8  => direct_emit(i32u8),
            U16 => (),
            U32 => (),
            U64 => direct_emit(i32i64),
            F32 => direct_emit(i32f32),
            F64 => direct_emit(i32f64),
        }
        U32 => match to {
            I8  => direct_emit(i32i8),
            I16 => direct_emit(i32i16),
            I32 => (),
            I64 => direct_emit(u32i64),
            U8  => direct_emit(i32u8),
            U16 => direct_emit(i32u16),
            U32 => (),
            U64 => direct_emit(u32i64),
            F32 => direct_emit(u32f32),
            F64 => direct_emit(u32f64),
        }
        U64 => match to {
            I8  => direct_emit(i32i8),
            I16 => direct_emit(i32i16),
            I32 => (),
            I64 => (),
            U8  => direct_emit(i32u8),
            U16 => direct_emit(i32u16),
            U32 => (),
            U64 => (),
            F32 => direct_emit(u64f32),
            F64 => direct_emit(u64f64),
        }
        F32 => match to {
            I8  => direct_emit(f32i8),
            I16 => direct_emit(f32i16),
            I32 => direct_emit(f32i32),
            I64 => direct_emit(f32i64),
            U8  => direct_emit(f32u8),
            U16 => direct_emit(f32u16),
            U32 => direct_emit(f32u32),
            U64 => direct_emit(f32u64),
            F32 => (),
            F64 => direct_emit(f32f64),
        }
        F64 => match to {
            I8  => direct_emit(f64i8),
            I16 => direct_emit(f64i16),
            I32 => direct_emit(f64i32),
            I64 => direct_emit(f64i64),
            U8  => direct_emit(f64u8),
            U16 => direct_emit(f64u16),
            U32 => direct_emit(f64u32),
            U64 => direct_emit(f64u64),
            F32 => direct_emit(f64f32),
            F64 => (),
        }
    }
}

fn load_according_to_type(ty: &Type) {
    if matches!(ty, ArrayOf(..) | Struct(..) | Union(..) | Func{..}) {return;}

    if *ty == Type::Float {
        emit!("  movss (%rax), %xmm0");
        return;
    }
    if *ty == Type::Double {
        emit!("  movsd (%rax), %xmm0");
        return;
    }

    let mov_kind = if ty.is_unsigned() {"movz"} else {"movs"};
    match sizeof(ty) {
        1 => emit!("  {}bl (%rax), %eax", mov_kind),
        2 => emit!("  {}wl (%rax), %eax", mov_kind),
        4 => emit!("  movsxd (%rax), %rax"),
        _ => emit!("  mov    (%rax), %rax"),
    }
}

fn error_expr(expr: &Expr, info: &str) -> String {
    let span = expr.span;
    let mut err_msg = String::new();
    let (start_line, start_column, end_line, end_column) = span.locate();
    let extended_error_info = format!(":{}:{}: {}\n", start_line, start_column, info.red());
    err_msg.push_str(&extended_error_info);
    let start_line_content = get_src_content_at_line(start_line);
    err_msg.push_str(&start_line_content);
    err_msg.push_str("\n");
    let spaces = " ".repeat(start_column - 1);
    let arrows = if start_line == end_line {
        "^".repeat(span.end_index - span.start_index + 1)
    } else {
        "^".to_string()
    };
    err_msg.push_str(&format!("{}{}", spaces, arrows.red()));
    err_msg
}
