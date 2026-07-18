use std::{io::{self, Write}, collections::VecDeque, process::exit, fs::File, sync::Mutex};
use std::cell::Cell;
use colored::*;
use crate::ir::ExprType::{self, *};
use crate::ir::StmtType::{self, *};
use crate::ir::OP::{self, *};
use ir::CompareToken::{self, *};
use crate::Declaration;
use crate::Function;
use crate::Obj;
use crate::Type::{self, *};
use crate::SRC;
use crate::INPUT_PATH;
use crate::ir::{self, *};
use crate::analyze::{self, *};
use crate::common::{self, *};

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

#[derive(Debug, Clone, PartialEq)]
pub enum Fundemental_Type {
    I8, I16, I32, I64,
}
use Fundemental_Type::*;

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
        for global_decl in &program.global_decls {
            emit!("  .data");
            emit!("  .globl {}", global_decl.obj.name);
            emit!("{}:", global_decl.obj.name);
            if let Some(bytes) = &global_decl.init_value {
                for b in bytes {
                    emit!("  .byte {}", b);
                }
            } else {
                // @Temporary: Before really implementing uninitialzed global variable (which is designated to .bss),
                // we just manually set its content to all zero in .data section.
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
        let mut i = 0;
        for param in &fun.params {
            match sizeof(&param.ty) {
                1 => emit!("  mov {}, -{}(%rbp)\n", self.argregs8[i],  fun.stack_size-param.offset),
                2 => emit!("  mov {}, -{}(%rbp)\n", self.argregs16[i], fun.stack_size-param.offset),
                4 => emit!("  mov {}, -{}(%rbp)\n", self.argregs32[i], fun.stack_size-param.offset),
                _ => emit!("  mov {}, -{}(%rbp)\n", self.argregs64[i], fun.stack_size-param.offset),
            }
            i += 1;
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
            ir::StmtType::Return(expr) =>self.ret_gen(&expr),
            ir::StmtType::Block(item) =>self.block_gen(item),
            ir::StmtType::If{cond, then, otherwise} => self.if_gen(cond, then, otherwise),
            ir::StmtType::For{init, cond, inc, then} => self.for_gen(init, cond, inc, then),
        }
    }

    fn ret_gen(&mut self, expr: &ir::Expr) {
        self.expr_gen(expr);
        emit!("  jmp .L.return.{}\n", self.cur_function_name);
    }

    fn if_gen(&mut self, cond: &ir::Expr, then: &ir::StmtType, otherwise: &Option<Box<ir::StmtType>>) {
        let c = self.next_jump_label_count();
        self.expr_gen(&cond);
        emit!("  cmp $0, %rax");
        emit!("  je  .L.else.{}", c);
        self.stmt_gen(&then);
        emit!("  jmp .L.end.{}", c);
        emit!(".L.else.{}:", c);
        if let Some(els) = otherwise {
            self.stmt_gen(&els);
        }
        emit!(".L.end.{}:", c);
    }

    fn for_gen(&mut self, init: &Option<ir::Expr>, cond: &Option<ir::Expr>, inc: &Option<ir::Expr>, then: &Box<ir::StmtType>) {
        let c = self.next_jump_label_count();
        if let Some(expr) = init {
            self.expr_gen(expr);
        }
        emit!(".L.begin.{}:", c);
        if let Some(expr) = cond {
            self.expr_gen(expr);
            emit!("  cmp $0, %rax");
            emit!("  je  .L.end.{}", c);
        }
        self.stmt_gen(&then);
        if let Some(expr) = inc {
            self.expr_gen(expr);
        }
        emit!("  jmp .L.begin.{}", c);
        emit!(".L.end.{}:", c);
    }

    fn expr_gen(&mut self, expr: &ir::Expr) {
        emit!("  .loc 1 {}", expr.span.get_start_line());
        let content = &expr.content;
        match content {
            Natural_Number(n) => emit!("  mov ${}, %rax", n),
            CommaExpression(lhs, rhs) => {
                self.expr_gen(lhs);
                self.expr_gen(rhs);
            }
            Binary(lhs, rhs, kind) => {
                self.expr_gen(rhs);
                self.push("%rax");
                self.expr_gen(lhs);
                self.pop("%rdi");
                let (ax, di) = if matches!(lhs.ty, Long | Pointer_To(..) | ArrayOf(..)) {
                    ("%rax", "%rdi")
                } else {
                    ("%eax", "%edi")
                };
                match kind {
                    Plus =>  emit!("  add {}, {}", di, ax),
                    Minus => emit!("  sub {}, {}", di, ax),
                    Mul =>   emit!("  imul {}, {}", di, ax),
                    Div => {
                        if sizeof(&lhs.ty) == 8 {
                            emit!("  cqo");
                        } else {
                            emit!("  cdq");
                        }
                        emit!("  idiv {}", di);
                    },
                    Compare(c) => {
                        emit!("  cmp {}, {}", di, ax);
                        match c {
                            Eq => emit!("  sete %al"),
                            Neq => emit!("  setne %al"),
                            LT => emit!("  setl %al"),
                            LE => emit!("  setle %al"),
                            GT => {
                                emit!("  cmp %rax, %rdi");
                                emit!("  setl %al");
                            },
                            GE => {
                                emit!("  cmp %rax, %rdi");
                                emit!("  setle %al");
                            },
                        }
                        emit!("  movzb %al, %rax");
                    },
                    _ => eprintln!("gen_code error: not support {:?}", content),
                }
            }
            Assign(var, val) => {
                self.gen_addr(var);
                self.push("%rax");
                self.expr_gen(val);
                self.store_according_to_type(&var.ty);
            }
            Neg(expr) => {
                self.expr_gen(expr);
                emit!("  neg %rax");
            }
            Deref(inner_expr) => {
                self.expr_gen(inner_expr);
                load_according_to_type(&expr.ty);
            }
            AddrOf(expr) => self.gen_addr(expr),
            Ident(obj) => {
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
            FunCall(func_ref, args) => {
                match &func_ref.content {
                    Ident(obj) => {
                        let mut nargs = 0;
                        for arg in args {
                            self.expr_gen(arg);
                            self.push("%rax");
                            nargs += 1;
                        }
                        // put arguments in designated registers
                        for i in (0..nargs).rev() {
                            self.pop(self.argregs64[i]);
                        }

                        // The x86-64 ABI requires RSP to be 16-byte aligned
                        // at the point of a `call`. If an odd number of
                        // 8-byte values are live on the stack, realign first.
                        let needs_align = self.depth % 2 == 1;
                        if needs_align {
                            emit!("  sub $8, %rsp");
                        }
                        emit!("  mov $0, %rax");
                        emit!("  call {}", obj.name);
                        if needs_align {
                            emit!("  add $8, %rsp");
                        }
                    }
                    _ => eprintln!("currently only support function name as call reference"),
                }
            }
            StmtExpr(stmts) => self.block_gen(stmts),
            _ => eprintln!("gen_code error: not support {:?}", content),
        }
    }

    fn next_jump_label_count(&mut self) -> usize {
        self.jump_label_count += 1;
        return self.jump_label_count;
    }

    fn gen_addr(&mut self, expr: &Expr) {
        match &expr.content {
            Ident(obj) => {
                if obj.is_global {
                    emit!("  lea {}(%rip), %rax", obj.name);
                } else {
                    emit!("  lea -{}(%rbp), %rax", self.cur_function_stack_size-obj.offset);
                }

            },
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
            _ => {
                let err_msg = error_expr(expr, "codegen error: can't get addr of this expr");
                eprintln!("{}", err_msg);
                exit(1);
            },
        }
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

        if matches!(ty, Struct(..)|Union(..)) {
            for i in 0..sizeof(ty) {
                emit!("  mov {}(%rax), %r8b", i);
                emit!("  mov %r8b, {}(%rdi)", i);
            }
        } else {
            match sizeof(ty) {
                1 => emit!("  mov  %al, (%rdi)"),
                2 => emit!("  mov  %ax, (%rdi)"),
                4 => emit!("  mov %eax, (%rdi)"),
                _ => emit!("  mov %rax, (%rdi)"),
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

    let from_fundemental_type = get_assembly_type(from);
    let to_fundemental_type   = get_assembly_type(to);
    gen_cast_operation(from_fundemental_type, to_fundemental_type);
}

fn cmp_zero(ty: &Type) {
  if is_integer(ty) && sizeof(ty) <= 4 {
    emit!("  cmp $0, %eax");
  } else {
    emit!("  cmp $0, %rax");
    }
}

fn get_assembly_type(ty: &Type) -> Fundemental_Type {
    match ty {
        // Bool is special. Because it is normalized to either 0 or 1, we can
        // just use I64 without any problem.
        Bool => I64,
        Char => I8,
        Short => I16,
        Int => I32,
        Long | Pointer_To(..) | ArrayOf(..) => I64,
        Func{return_type, ..} => get_assembly_type(return_type),
        _ => {
            println!("cannot get the fundemental type of this type: {:?}", ty);
            exit(1);
        }
    }
}

fn gen_cast_operation(from: Fundemental_Type, to: Fundemental_Type) {
    match from {
        I8 => match to {
                I32 => emit!("movsxd %eax, %rax"),
                _ => (),
            }
        I16 => match to {
                I8 => emit!("movsbl %al, %eax"),
                I64 => emit!("movsxd %eax, %rax"),
                _ => (),
        }
        I32 => match to {
                I8 => emit!("movsbl %al, %eax"),
                I16 => emit!("movswl %ax, %eax"),
                I64 => emit!("movsxd %eax, %rax"),
                _ => (),
        }
        I64 => match to {
                I8 => emit!("movsbl %al, %eax"),
                I16 => emit!("movswl %ax, %eax"),
                _ => (),
        }
    }
}

fn load_according_to_type(ty: &Type) {
    if matches!(ty, ArrayOf(..) | Struct(..) | Union(..)) {
        return;
    } else {
        match sizeof(ty) {
            1 => emit!("  movsbl (%rax), %eax"),
            2 => emit!("  movswl (%rax), %eax"),
            4 => emit!("  movsxd (%rax), %rax"),
            _ => emit!("  mov    (%rax), %rax"),
        }
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
