use std::{io::{self, Write}, collections::VecDeque, process::exit};
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

pub struct Generator {
    aprogram_r: ir::AnalyzedProgram,
    lable_count: Cell<i32>,
    cur_afun_r: ir::Function,
    argregs: Vec<&'static str>,
}

impl Generator {
    pub fn new(aprogram_r: ir::AnalyzedProgram) -> Self {
        let argregs = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        let cur_afun_r = aprogram_r.afuns[0].clone();
        Self {aprogram_r, lable_count: 0.into(), cur_afun_r, argregs}
    }

    pub fn gen_code(&mut self) {
        for global_decl in &self.aprogram_r.a_global_decls {
            println!("  .data");
            println!("  .globl {}", global_decl.obj.name);
            println!("{}:", global_decl.obj.name);
            println!("  .zero {}", sizeof(&global_decl.obj.ty));
        }
        for afun in &self.aprogram_r.afuns {
            // @Space: clone() wastes memory
            self.cur_afun_r = afun.clone();
            self.fun_gen();
        }
        println!("  .section .note.GNU-stack,\"\",@progbits");
    }

    pub fn fun_gen(&self) {
        let stack_size = self.cur_afun_r.stack_size;
        let fun = &self.cur_afun_r;
        let aligned_stack_size = align_to(stack_size, 16);
        // prologue
        println!();
        println!("  .globl {}", fun.name);
        println!("  .text");
        println!("{}:", fun.name);
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub ${}, %rsp", aligned_stack_size);
        println!();
        let mut i = 0;
        for param in &self.cur_afun_r.param_names {
            let obj = self.cur_afun_r.scope_tracker.resolve_symbol(&param);
                                                              // @Cleanup: offset should be expressed more properly
            println!("  mov {}, {}(%rbp)\n", self.argregs[i], -self.cur_afun_r.stack_size+obj.unwrap().offset);
            i += 1;
        }
        self.block_gen(&self.cur_afun_r.stmts);

        // end
        println!(".L.return.{}:",fun.name);
        println!("  mov %rbp, %rsp");
        println!("  pop %rbp");
        println!("  ret");
    }

    fn block_gen(&self, stmts: &Vec<ir::StmtType>) {
        for stmt in stmts {
            println!(";;;;;;;;;;;;;;;;;;;;;;;;");
            self.stmt_gen(stmt);
            print!(";;;;;;;;;;;;;;;;;;;;;;;;");
        }
        println!();
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
        println!("  jmp .L.return.{}\n", self.cur_afun_r.name);
    }

    fn if_gen(&self, cond: &ir::Expr, then: &ir::StmtType, otherwise: &Option<Box<ir::StmtType>>) {
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

    fn for_gen(&self, init: &Option<ir::Expr>, cond: &Option<ir::Expr>, inc: &Option<ir::Expr>, then: &Box<ir::StmtType>) {
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

    fn expr_gen(&self, expr: &ir::Expr) {
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
                store();
            }
            Neg(expr) => {
                self.expr_gen(expr);
                println!("  neg %rax");
            }
            Deref(inner_expr) => {
                self.expr_gen(inner_expr);
                load_according_to_type(&expr.ty);
            }
            AddrOf(expr) => self.gen_addr(expr),
            Ident(s) => {
                let err_smg = &format!("symbol '{}' not found\n", s);
                let obj = self.cur_afun_r.scope_tracker.resolve_symbol(s).expect(err_smg);
                self.gen_addr(expr);
                load_according_to_type(&expr.ty);
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
        let spaces = " ".repeat(expr.span.start_index);
        let arrows = "^".repeat(expr.span.end_index - expr.span.start_index);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }

    fn gen_addr_by_name(&self, name: &str) {
        let obj = self.cur_afun_r.scope_tracker.resolve_symbol(name);
        println!("  lea {}(%rbp), %rax", -self.cur_afun_r.stack_size+obj.unwrap().offset);
    }

    fn gen_addr(&self, expr: &Expr) {
        match &expr.content {
            Ident(name) => {
                let obj = self.cur_afun_r.scope_tracker.resolve_symbol(name).unwrap();
                if obj.is_global {
                    println!("  lea {}(%rip), %rax", obj.name);
                } else {
                                                 // @Cleanup: offset should be expressed more properly
                    println!("  lea {}(%rbp), %rax", -self.cur_afun_r.stack_size+obj.offset);
                }

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

fn load_according_to_type(ty: &Type) {
    if let ArrayOf(_, _) = ty {

    } else {
        println!("  mov (%rax), %rax");
    }
}

fn store() {
    println!("  pop %rdi");
    println!("  mov %rax, (%rdi)");
}

fn align_to(n: i32, align: i32) -> i32 {
    let extra = n % align;
    let base = n - extra;
    match extra {
        0 => base,
        _ => base + align,
    }
}
