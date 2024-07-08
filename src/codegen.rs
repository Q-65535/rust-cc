use std::{io::{self, Write}, process::exit};
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
    objs: Vec<Obj>,
    cur_offset: i32,
}

impl SblTable {
    fn new() -> Self {
        SblTable {objs: Vec::new(), cur_offset: 1}
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
        self.objs.push(o);
        self.cur_offset += 1;
    }
}

pub struct Generator {
    src: String,
    sbl_table: SblTable,
    lable_count: i32,
}

impl Generator {
    pub fn new(src: &String) -> Self {
        Self {src: src.clone(), sbl_table: SblTable::new(), lable_count: 0,}
    }
    
    pub fn gen_code(&mut self, stmts: &Vec<StmtType>) {
        let stack_size = self.stack_size(stmts);
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
                    let obj = match self.sbl_table.find_obj(&name) {
                        Some(o) => o,
                        None => {
                            self.sbl_table.add_new_obj(&name);
                            // @Performance: finding an obj in vec might take long time
                            self.sbl_table.find_obj(&name).expect("codegen fatal error, no symbol found in assignment")
                        },
                    };
                    gen_addr(obj);
                    println!("  push %rax");
                    self.expr_gen(val);
                    println!("  pop %rdi");
                    println!("  mov %rax, (%rdi)");
                } else {
                    println!("lval is not an identifier");
                    exit(0);
                }
            },
            Neg(expr) => {
                self.expr_gen(&expr);
                println!("  neg %rax");
            },
            Var(s) => {
                let err_smg = &format!("symbol '{}' not found\n", s);
                let obj = self.sbl_table.find_obj(&s).expect(err_smg);
                gen_addr(obj);
                println!("  mov (%rax), %rax\n");
            },
            _ => println!("gen_code error: not support {:?}", content),
        }
    }

    fn stack_size(&mut self, stmts: &Vec<StmtType>) -> i32 {
        self.scan_block(stmts);
        let len: i32 = self.sbl_table.objs.len().try_into().unwrap();
        let size: i32 = len * 8;
        self.sbl_table = SblTable::new();
        align_to(size, 16)
    }

    fn scan_block(&mut self, stmts: &Vec<StmtType>) {
        for stmt in stmts {
            match stmt {
                StmtType::Ex(expr) => self.scan_expr(expr),
                StmtType::Return(expr) => self.scan_expr(expr),
                StmtType::Block(stmts) => self.scan_block(stmts),
                _ => (),
            }
        }
    }

    fn scan_expr(&mut self, expr: &Expr) {
        match &expr.content {
            Binary(lhs, rhs, _) => {
                self.scan_expr(&rhs);
                self.scan_expr(&lhs);
            },
            // @Temporary: in the end, only declarations count for the stack size
            Assign(var, expr) => {
                self.scan_expr(expr);
                if let Var(name) = &var.content {
                    if let None = self.sbl_table.find_obj(&name) {
                        self.sbl_table.add_new_obj(&name);
                    }
                }
            },
            _ => (),
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
}

fn gen_addr(o: &Obj) {
        println!("  lea {}(%rbp), %rax", -o.offset*8);
}

fn align_to(n: i32, align: i32) -> i32 {
    let extra = n % align;
    let base = n - extra;
    match extra {
        0 => base,
        _ => base + align,
    }
}
