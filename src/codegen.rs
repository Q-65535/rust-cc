use std::{io::{self, Write}, process::exit};
use crate::ExprType::{self, *};
use crate::StmtType::{self, *};
use crate::TokenKind::{self, *};
use crate::CompareToken::{self, *};
use crate::Expr;

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
                    }
                    println!("  movzb %al, %rax");
                },
                _ => println!("gen_code error: not support {:?}", content),
            }
        }
        Assign(var, val) => {
            gen_addr(&var);
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
            gen_addr(&s);
            println!("  move (%rax), %rax\n");
        },
        _ => println!("gen_code error: not support {:?}", content),
    }
}

fn gen_addr(var: &str) {
    if let Some(c) = var.chars().next() {
        let cval = c as i32;
        let base = 'a' as i32;
        let offset = (cval - base + 1)*8;
        println!("  lea {}(%rbp), %rax", -offset);
    } else {
    println!("gen_addr error: 0 length var name: {}", var);
    }
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