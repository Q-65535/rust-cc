use std::{io::{self, Write}, collections::VecDeque, process::exit};
use std::cell::Cell;
use colored::*;
use crate::ExprType::{self, *};
use crate::StmtType::{self, *};
use crate::TokenKind::{self, *};
use crate::CompareToken::{self, *};
use crate::BlockItem::{self, *};
use crate::Declaration;
use crate::Expr;
use crate::Function;
use crate::Obj;
use crate::SblTable;
use crate::AnalyzedProgram;
use crate::AnalyzedFun;
use crate::SRC;

pub struct Generator {
    aprogram: AnalyzedProgram,
    lable_count: Cell<i32>,
    cur_afun: AnalyzedFun,
    argregs: Vec<&'static str>,
}

struct FunContext {
    cur_sbl_table: SblTable,
    cur_stack_size: i32,
    cur_fun_name: String,
}

impl Generator {
    // @Smell: Strange to use &String in rust
    pub fn new(aprogram: AnalyzedProgram) -> Self {
        let argregs = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        let cur_afun = aprogram.afuns[0].clone();
        Self {aprogram, lable_count: 0.into(), cur_afun, argregs}
    }

    pub fn gen_code(&mut self) {
        for afun in &self.aprogram.afuns {
            // @Space: clone() wastes memory
            self.cur_afun = afun.clone();
            self.fun_gen();
        }
    }
    
    pub fn fun_gen(&self) {
        let stack_size = self.cur_afun.stack_size;
        let fun = &self.cur_afun.fun;
        let aligned_stack_size = align_to(stack_size, 16);
        // prologue
        println!();
        println!("  .globl {}", fun.name);
        println!("{}:", fun.name);
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub ${}, %rsp", aligned_stack_size);
        println!();
        self.block_gen(&self.cur_afun.fun.items);

        // end
        println!(".L.return.{}:",fun.name);
        println!("  mov %rbp, %rsp");
        println!("  pop %rbp");
        println!("  ret");
    }

    fn block_gen(&self, items: &Vec<BlockItem>) {
        for item in items {
            match item {
                Stmt(stmt) => self.stmt_gen(stmt),
                Decl(decl) => self.decl_gen(decl),
            }
        }
        println!();
    }

    fn stmt_gen(&self, stmt: &StmtType) {
        match stmt {
            StmtType::Ex(expr) => self.expr_gen(&expr),
            StmtType::Return(expr) =>self.ret_gen(&expr),
            StmtType::Block(item) =>self.block_gen(item),
            StmtType::If{cond, then, otherwise} => self.if_gen(cond, then, otherwise),
            StmtType::For{init, cond, inc, then} => self.for_gen(init, cond, inc, then),
        }
    }

    fn decl_gen(&self, decl: &Declaration) {
        for init in &decl.init_declarators {
            if let Some(expr) = &init.init_expr {
                self.gen_addr_by_name(&init.declarator.name);
                println!("  push %rax");
                self.expr_gen(expr);
                println!("  pop %rdi");
                println!("  mov %rax, (%rdi)");
            }
        }
    }

    fn ret_gen(&self, expr: &Expr) {
        self.expr_gen(expr);
        println!("  jmp .L.return.{}\n", self.cur_afun.fun.name);
    }

    fn if_gen(&self, cond: &Expr, then: &StmtType, otherwise: &Option<Box<StmtType>>) {
        let c = self.count();
        self.expr_gen(&cond);
        println!("  cmp $0, %rax");
        println!("  je  .L.else.{}", c);
        self.stmt_gen(&then);
        println!("  jmp .L.end.{}", c);
        println!(".L.else.{}:", c);
        if let Some(els) = otherwise {
            self.stmt_gen(&els);
        }
        println!(".L.end.{}:", c);
    }

    fn for_gen(&self, init: &Option<Expr>, cond: &Option<Expr>, inc: &Option<Expr>, then: &Box<StmtType>) {
        let c = self.count();
        if let Some(expr) = init {
            self.expr_gen(expr);
        }
        println!(".L.begin.{}:", c);
        if let Some(expr) = cond {
            self.expr_gen(expr);
            println!("  cmp $0, %rax");
            println!("  je  .L.end.{}", c);
        }
        self.stmt_gen(&then);
        if let Some(expr) = inc {
            self.expr_gen(expr);
        }
        println!("  jmp .L.begin.{}", c);
        println!(".L.end.{}:", c);
    }

    fn expr_gen(&self, expr: &Expr) {
        let content = &expr.content;
        match content {
            Number(n) => println!("  mov ${}, %rax", n),
            Binary(lhs, rhs, kind) => {
                self.expr_gen(rhs);
                println!("  push %rax");
                self.expr_gen(lhs);
                println!("  pop %rdi");
                match kind {
                    Plus => println!("  add %rdi, %rax"),
                    Minus => println!("  sub %rdi, %rax"),
                    Mul => println!("  imul %rdi, %rax"),
                    Div => {
                        println!("  cqo");
                        println!("  idiv %rdi");
                    },
                    Compare(c) => {
                        println!("  cmp %rdi, %rax");
                        match c {
                            Eq => println!("  sete %al"),
                            Neq => println!("  setne %al"),
                            LT => println!("  setl %al"),
                            LE => println!("  setle %al"),
                            GT => {
                                println!("  cmp %rax, %rdi");
                                println!("  setl %al");
                            },
                            GE => {
                                println!("  cmp %rax, %rdi");
                                println!("  setle %al");
                            },
                        }
                        println!("  movzb %al, %rax");
                    },
                    _ => println!("gen_code error: not support {:?}", content),
                }
            }
            Assign(var, val) => {
                self.gen_addr(var);
                println!("  push %rax");
                self.expr_gen(val);
                println!("  pop %rdi");
                println!("  mov %rax, (%rdi)");
            }
            Neg(expr) => {
                self.expr_gen(expr);
                println!("  neg %rax");
            }
            Deref(expr) => {
                self.expr_gen(expr);
                println!("  mov (%rax), %rax");
            }
            AddrOf(expr) => self.gen_addr(expr),
            Ident(s) => {
                let err_smg = &format!("symbol '{}' not found\n", s);
                let obj = self.cur_afun.sbl_table.find_obj(s).expect(err_smg);
                self.gen_addr(expr);
                println!("  mov (%rax), %rax\n");
            }
            FunCall(func_ref, args) => {
                match &func_ref.content {
                    Ident(func_name) => {
                        let mut nargs = 0;
                        for arg in args {
                            self.expr_gen(arg);
                            println!("  push %rax");
                            nargs += 1;
                        }
                        // put arguments in designated registers
                        for i in (0..nargs).rev() {
                            self.pop(self.argregs[i]);
                        }

                        println!("  mov $0, %rax");
                        println!("  call {}", func_name);
                    }
                    // @Robustness: improve error message
                    _ => println!("currently only support function name as call reference"),
                }
            }
            _ => println!("gen_code error: not support {:?}", content),
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
        let spaces = " ".repeat(expr.start);
        let arrows = "^".repeat(expr.end - expr.start);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }

    fn gen_addr_by_name(&self, name: &str) {
        let obj = self.cur_afun.sbl_table.find_obj(name);
        println!("  lea {}(%rbp), %rax", -self.cur_afun.stack_size+obj.unwrap().offset);
    }

    fn gen_addr(&self, expr: &Expr) {
        match &expr.content {
            Ident(name) => {
                let obj = self.cur_afun.sbl_table.find_obj(name);
                println!("  lea {}(%rbp), %rax", -self.cur_afun.stack_size+obj.unwrap().offset);

            },
            Deref(expr) => {
                self.expr_gen(expr);
            },
            _ => {
                let err_msg = self.error_expr(expr, "can't get addr of this expr");
                println!("{}", err_msg);
            },
        }
    }

    fn push(&self, reg: &str) {
        println!("  push {}", reg);
    }

    fn pop(&self, reg: &str) {
        println!("  pop {}", reg);
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
