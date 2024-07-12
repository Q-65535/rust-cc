use std::{io::{self, Write}, collections::VecDeque, process::exit};
use colored::*;
use crate::ExprType::{self, *};
use crate::StmtType::{self, *};
use crate::TokenKind::{self, *};
use crate::CompareToken::{self, *};
use crate::Expr;

// @temporal: each obj occupy 8 bytes
#[derive(Debug, Clone, PartialEq)]
struct Obj {
    name: String,
    offset: i32,
}

struct SblTable {
    objs: VecDeque<Obj>,
    cur_offset: i32,
    stack_size: i32,
}

impl SblTable {
    fn new() -> Self {
        SblTable {objs: VecDeque::new(), cur_offset: 0, stack_size: 0}
    }

    fn find_obj(&self, s: &str) -> Option<&Obj> {
        for o in &self.objs {
            if o.name == s {
                return Some(o);
            }
        }
        None
    }

    fn add_new_obj(&mut self, name: &str) {
        let o = Obj{name: name.to_string(), offset: self.cur_offset};
        self.objs.push_front(o);
        self.cur_offset += 8;
    }
}

pub struct Generator {
    src: String,
    sbl_table: SblTable,
    lable_count: i32,
    var_count: i32,
}

impl Generator {
    pub fn new(src: &String, var_count: i32) -> Self {
        Self {src: src.clone(), sbl_table: SblTable::new(), lable_count: 0, var_count}
    }
    
    pub fn gen_code(&mut self, stmts: &Vec<StmtType>) {
        let stack_size = self.var_count * 8;
        let aligned_stack_size = align_to(stack_size, 16);
        self.sbl_table.stack_size = stack_size;
        // prologue
        println!("  .globl main");
        println!("main:");
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub ${}, %rsp", stack_size);
        println!();

        self.block_gen(stmts);

        // end
        println!(".L.return:");
        println!("  mov %rbp, %rsp");
        println!("  pop %rbp");
        println!("  ret");
    }

    fn block_gen(&mut self, stmts: &Vec<StmtType>) {
        for stmt in stmts {
            self.stmt_gen(stmt);
        }
        println!();
    }

    fn stmt_gen(&mut self, stmt: &StmtType) {
        match stmt {
            StmtType::Ex(expr) => self.expr_gen(&expr),
            StmtType::Return(expr) =>self.ret_gen(&expr),
            StmtType::Block(stmts) =>self.block_gen(stmts),
            StmtType::If{cond, then, otherwise} => self.if_gen(cond, then, otherwise),
            StmtType::For{init, cond, inc, then} => self.for_gen(init, cond, inc, then),
            _ => println!("currently not support {:?}", stmt),
        }
    }

    fn ret_gen(&mut self, expr: &Expr) {
        self.expr_gen(expr);
        println!("  jmp .L.return\n");
    }

    fn if_gen(&mut self, cond: &Expr, then: &StmtType, otherwise: &Option<Box<StmtType>>) {
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

    fn for_gen(&mut self, init: &Option<Expr>, cond: &Option<Expr>, inc: &Option<Expr>, then: &Box<StmtType>) {
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

    fn expr_gen(&mut self, expr: &Expr) {
        let content = &expr.content;
        match content {
            Number(n) => println!("  mov ${}, %rax", n),
            Binary(lhs, rhs, kind) => {
                self.expr_gen(rhs);
                println!("  push %rax");
                self.expr_gen(lhs);
                println!("  pop %rdi");
                match kind  {
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
                if let Var(name) = &var.content {
                    if let None = self.sbl_table.find_obj(name) {
                        self.sbl_table.add_new_obj(name);
                    }
                }
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
            Var(s) => {
                let err_smg = &format!("symbol '{}' not found\n", s);
                let obj = self.sbl_table.find_obj(s).expect(err_smg);
                self.gen_addr(expr);
                println!("  mov (%rax), %rax\n");
            }
            _ => println!("gen_code error: not support {:?}", content),
        }
    }

    fn count(&mut self) -> i32 {
        self.lable_count += 1;
        self.lable_count
    }

    fn error_expr(&self, expr: &Expr, info: &str) -> String {
        let mut err_msg = String::from("");
        err_msg.push_str(&format!("{}\n", self.src));
        let spaces = " ".repeat(expr.start);
        let arrows = "^".repeat(expr.end - expr.start);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }

    fn gen_addr(&mut self, expr: &Expr) {
        match &expr.content {
            Var(name) => {
                let obj = self.sbl_table.find_obj(name);
                println!("  lea {}(%rbp), %rax", -self.sbl_table.stack_size+obj.unwrap().offset);
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
}

fn align_to(n: i32, align: i32) -> i32 {
    let extra = n % align;
    let base = n - extra;
    match extra {
        0 => base,
        _ => base + align,
    }
}
