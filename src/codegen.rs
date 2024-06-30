use std::{io::{self, Write}, process::exit};
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
    sbl_table: SblTable,
}

impl Generator {
    pub fn new() -> Self {
        Self {sbl_table: SblTable::new()}
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

        for stmt in stmts {
            match stmt {
                StmtType::Ex(expr) => self.expr_gen(&expr),
                StmtType::Return(expr) =>self.ret_gen(&expr),
            }
            println!();
        }

        // end
        println!(".L.return:");
        println!("  mov %rbp, %rsp");
        println!("  pop %rbp");
        println!("  ret");
    }

    fn ret_gen(&mut self, expr: &Expr) {
        self.expr_gen(expr);
        println!("  jmp .L.return\n");
    }

    fn expr_gen(&mut self, expr: &Expr) {
        let content = &expr.content;
        match content {
            Number(n) => println!("  mov ${}, %rax", n),
            Binary(lhs, rhs, kind) => {
                self.expr_gen(&rhs);
                println!("  push %rax");
                self.expr_gen(&lhs);
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
                let obj = match self.sbl_table.find_obj(&var) {
                    Some(o) => o,
                    None => {
                        self.sbl_table.add_new_obj(&var);
                        // @Performance: finding the new obj in vec takes long time
                        self.sbl_table.find_obj(&var).expect("codegen fatal error, no symbol found in assignment")
                    },
                };
                gen_addr(obj);
                println!("  push %rax");
                self.expr_gen(val);
                println!("  pop %rdi");
                println!("  mov %rax, (%rdi)");
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
        for stmt in stmts {
            match stmt {
                StmtType::Ex(expr) => self.scan_expr(expr),
                StmtType::Return(expr) => self.scan_expr(expr),
            }
        }
        let len: i32 = self.sbl_table.objs.len().try_into().unwrap();
        let size: i32 = len * 8;
        self.sbl_table = SblTable::new();
        align_to(size, 16)
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
                match self.sbl_table.find_obj(&var) {
                    None => self.sbl_table.add_new_obj(&var),
                    _ => (),
                }
            },
            _ => (),
        }
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
