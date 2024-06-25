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

static mut cur_offset: i32 = 1;

// basically a symbol table
static objs: Vec<Obj> = Vec::new();

pub fn gen_code(stmts: Vec<StmtType>) {
    println!("assembly result:");
    println!("  .globl main");
    println!("main:");
        for stmt in stmts {
            match stmt {
                StmtType::Ex(expr) => {
                    expr_gen(&expr);
                }
            }
            println!();
        }
    println!("  ret");
}

fn expr_gen(expr: &Expr) {
    let mut sbl_table = SblTable::new();
    let content = &expr.content;
    match content {
        Number(n) => println!("  mov ${}, %rax", n),
        Binary(lhs, rhs, kind) => {
            expr_gen(&rhs);
            println!("  push %rax");
            expr_gen(&lhs);
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
                        LT => println!("  selt %al"),
                        LE => println!("  sele %al"),
                        GT => {
                            println!("  sele %al");
                            println!("  neg %al");
                        },
                        GE => {
                            println!("  selt %al");
                            println!("  neg %al");
                        },
                    }
                    println!("  movzb %al, %rax");
                },
                _ => println!("gen_code error: not support {:?}", content),
            }
        }
        Assign(var, val) => {
            let obj = match sbl_table.find_obj(&var) {
                Some(o) => o,
                None => {
                    sbl_table.add_new_obj(&var);
                    sbl_table.find_obj(&var).expect("codegen fatal error, no symbol found in assignment")
                },
            };
            // gen_addr for an obj
            gen_addr(obj);

            println!("  push %rax");
            expr_gen(val);
            println!("  pop %rdi");
            println!("  mov %rax, (%rdi)");
        },
        Neg(expr) => {
            expr_gen(&expr);
            println!("  neg %rax");
        },
        Var(s) => {
            let err_smg = &format!("symbol '{}' not found\n", s);
            let obj = sbl_table.find_obj(&s).expect(err_smg);
            gen_addr(obj);
            println!("  move (%rax), %rax\n");
        },
        _ => println!("gen_code error: not support {:?}", content),
    }
}

fn gen_addr(o: &Obj) {
        println!("  lea {}(%rbp), %rax", -o.offset*8);
}

fn show_expr(root: &Expr) {
    match &root.content {
        Number(n) => print!("{}", n),
        Binary(lhs, rhs, kind) => {
            print!("(");
            show_expr(&lhs);
            print!("{}", root.token.content);
            show_expr(&rhs);
            print!(")");
        },
        Neg(expr) => {
            print!("-");
            show_expr(&expr);
        },
        _ => println!("show_expr error, not support showing {:?}", &root.content),
    }
}