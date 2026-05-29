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
use crate::ir::{self, *};
use crate::analyze::{self, *};

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
    aprogram_r: ir::AnalyzedProgram,
    lable_count: Cell<i32>,
    cur_afun_r: Option<ir::Function>,
    argregs64: Vec<&'static str>,
    argregs8: Vec<&'static str>,
}

impl Generator {
    pub fn new(aprogram_r: ir::AnalyzedProgram) -> Self {
        let argregs64 = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        let argregs8  = vec!["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
        Self {aprogram_r, lable_count: 0.into(), cur_afun_r: None, argregs64, argregs8}
    }

    // The function currently being emitted. Only set while `gen_code`
    // iterates `afuns`, which is the only context these reads happen in.
    fn cur_afun(&self) -> &ir::Function {
        self.cur_afun_r.as_ref().unwrap()
    }

    pub fn gen_code(&mut self) {
        for global_decl in &self.aprogram_r.global_decls {
            emit!("  .data");
            emit!("  .globl {}", global_decl.obj.name);
            emit!("{}:", global_decl.obj.name);
            if let Some(bytes) = &global_decl.init_value {
                for b in bytes {
                    emit!("  .byte {}", b);
                }
            } else {
                emit!("  .zero {}", sizeof(&global_decl.obj.ty));
            }
        }
        for afun in &self.aprogram_r.afuns {
            // @Space: clone() wastes memory
            self.cur_afun_r = Some(afun.clone());
            self.fun_gen();
        }
        emit!("  .section .note.GNU-stack,\"\",@progbits");
    }

    pub fn fun_gen(&self) {
        let stack_size = self.cur_afun().stack_size;
        let fun = self.cur_afun();
        let aligned_stack_size = align_to(stack_size, 16);
        // prologue
        emit!();
        emit!("  .globl {}", fun.name);
        emit!("  .text");
        emit!("{}:", fun.name);
        emit!("  push %rbp");
        emit!("  mov %rsp, %rbp");
        emit!("  sub ${}, %rsp", aligned_stack_size);
        emit!();
        let mut i = 0;
        for param in &self.cur_afun().param_names {
            let obj = self.cur_afun().scope_tracker.resolve_symbol(&param).unwrap();
            match sizeof(&obj.ty) {
                1 => emit!("  mov {}, {}(%rbp)\n", self.argregs8[i], -self.cur_afun().stack_size+obj.offset),
                _ => emit!("  mov {}, {}(%rbp)\n", self.argregs64[i], -self.cur_afun().stack_size+obj.offset),
            }
            i += 1;
        }
        self.block_gen(&self.cur_afun().stmts);

        // end
        emit!(".L.return.{}:",fun.name);
        emit!("  mov %rbp, %rsp");
        emit!("  pop %rbp");
        emit!("  ret");
    }

    fn block_gen(&self, stmts: &Vec<ir::StmtType>) {
        for stmt in stmts {
            emit!(";;;;;;;;;;;;;;;;;;;;;;;;");
            self.stmt_gen(stmt);
            emit_raw!(";;;;;;;;;;;;;;;;;;;;;;;;");
        }
        emit!();
    }

    fn stmt_gen(&self, stmt: &ir::StmtType) {
        match stmt {
            ir::StmtType::Ex(expr) => self.expr_gen(&expr),
            ir::StmtType::Return(expr) =>self.ret_gen(&expr),
            ir::StmtType::Block(item) =>self.block_gen(item),
            ir::StmtType::If{cond, then, otherwise} => self.if_gen(cond, then, otherwise),
            ir::StmtType::For{init, cond, inc, then} => self.for_gen(init, cond, inc, then),
        }
    }

    fn ret_gen(&self, expr: &ir::Expr) {
        self.expr_gen(expr);
        emit!("  jmp .L.return.{}\n", self.cur_afun().name);
    }

    fn if_gen(&self, cond: &ir::Expr, then: &ir::StmtType, otherwise: &Option<Box<ir::StmtType>>) {
        let c = self.count();
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

    fn for_gen(&self, init: &Option<ir::Expr>, cond: &Option<ir::Expr>, inc: &Option<ir::Expr>, then: &Box<ir::StmtType>) {
        let c = self.count();
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

    fn expr_gen(&self, expr: &ir::Expr) {
        let content = &expr.content;
        match content {
            Number(n) => emit!("  mov ${}, %rax", n),
            Binary(lhs, rhs, kind) => {
                self.expr_gen(rhs);
                emit!("  push %rax");
                self.expr_gen(lhs);
                emit!("  pop %rdi");
                match kind {
                    Plus => emit!("  add %rdi, %rax"),
                    Minus => emit!("  sub %rdi, %rax"),
                    Mul => emit!("  imul %rdi, %rax"),
                    Div => {
                        emit!("  cqo");
                        emit!("  idiv %rdi");
                    },
                    Compare(c) => {
                        emit!("  cmp %rdi, %rax");
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
                emit!("  push %rax");
                self.expr_gen(val);
                store(&var.ty);
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
            FunCall(func_ref, args) => {
                match &func_ref.content {
                    Ident(obj) => {
                        let mut nargs = 0;
                        for arg in args {
                            self.expr_gen(arg);
                            emit!("  push %rax");
                            nargs += 1;
                        }
                        // put arguments in designated registers
                        for i in (0..nargs).rev() {
                            self.pop(self.argregs64[i]);
                        }

                        emit!("  mov $0, %rax");
                        emit!("  call {}", obj.name);
                    }
                    // @Robustness: improve error message
                    _ => eprintln!("currently only support function name as call reference"),
                }
            }
            StmtExpr(stmts) => self.block_gen(stmts),
            _ => eprintln!("gen_code error: not support {:?}", content),
        }
    }

    fn count(&self) -> i32 {
        self.lable_count.set(self.lable_count.get()+1);
        self.lable_count.get()
    }

    fn error_expr(&self, expr: &Expr, info: &str) -> String {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(expr.span.start_index);
        let arrows = "^".repeat(expr.span.end_index - expr.span.start_index);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }

    fn gen_addr_by_name(&self, name: &str) {
        let obj = self.cur_afun().scope_tracker.resolve_symbol(name).unwrap();
        emit!("  lea {}(%rbp), %rax", -self.cur_afun().stack_size+obj.offset);
    }

    fn gen_addr(&self, expr: &Expr) {
        match &expr.content {
            Ident(obj) => {
                if obj.is_global {
                    emit!("  lea {}(%rip), %rax", obj.name);
                } else {
                    emit!("  lea {}(%rbp), %rax", -self.cur_afun().stack_size+obj.offset);
                }

            },
            Deref(expr) => {
                self.expr_gen(expr);
            },
            _ => {
                let err_msg = self.error_expr(expr, "can't get addr of this expr");
                eprintln!("{}", err_msg);
            },
        }
    }

    fn push(&self, reg: &str) {
        emit!("  push {}", reg);
    }

    fn pop(&self, reg: &str) {
        emit!("  pop {}", reg);
    }
}

fn load_according_to_type(ty: &Type) {
    if let ArrayOf(_, _) = ty {

    } else {
        match sizeof(ty) {
            1 => emit!("  movsbq (%rax), %rax"),
            _ => emit!("  mov (%rax), %rax"),
        }
    }
}

fn store(ty: &Type) {
    emit!("  pop %rdi");
    match sizeof(ty) {
        1 => emit!("  mov %al, (%rdi)"),
        _ => emit!("  mov %rax, (%rdi)"),
    }
}

fn align_to(n: i32, align: i32) -> i32 {
    let extra = n % align;
    let base = n - extra;
    match extra {
        0 => base,
        _ => base + align,
    }
}
