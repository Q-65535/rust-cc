 #![allow(warnings)]
use std::{io::{self, Write}, process::exit};
use colored::*;
use crate::lex::*;
use crate::parse::*;
use crate::TokenKind::*;
use crate::CompareToken::*;
use crate::ExprType::*;
pub mod parse;
pub mod lex;

// static global_src: &str = "-10-1234*(2-3423)/11112;1+2;";
static global_src: &str = "c = 1; b = 10;";



fn main() {
    println!("compiling source code: '{global_src}'");
    let mut lexer = Lexer::new(global_src);

    let mut tokens: Vec<Token> = Vec::new();
    let tokens = lexer.lex();
    let src = global_src.to_string().clone();
    let mut parser = Parser::new(&src, tokens, 0);
    let res = parser.parse();

    println!();
    println!("assembly result:");
    println!();
    println!("  .globl main");
    println!("main:");
    match res {
        Ok(program) => {
            for stmt in program {
                match stmt {
                    StmtType::Ex(expr) => {
                        gen_code(&expr);
                    }
                }
                println!();
            }
        },
        Err(err_msg) => {
            println!("{}", err_msg);
        },
    }
    println!("  ret");
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

fn gen_code(expr: &Expr) {
    let content = &expr.content;
    match content {
        Number(n) => println!("  mov ${}, %rax", n),
        Binary(lhs, rhs, kind) => {
            gen_code(&rhs);
            println!("  push %rax");
            gen_code(&lhs);
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
                _ => {
                    let err_msg = "aa";//error_token(global_src, &expr.token, "unknown binary token");
                    println!("{}", err_msg);
                },
            }
        }
        Assign(var, val) => {
            gen_addr(&var);
            println!("  push %rax");
            gen_code(val);
            println!("  pop %rdi");
            println!("  mov %rax, (%rdi)");
        },
        Neg(expr) => {
            gen_code(&expr);
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
        println!("  lea {}(%rbp), %rax\n", -offset);
    } else {
    println!("gen_addr error: 0 length var name: {}", var);
    }
}

fn error_at(src: &String, index: usize, err_msg: &str) {
    println!("{}", src);
    let spaces = " ".repeat(index);
    println!("{}^ {}", spaces, err_msg.red());
    exit(0);
}

fn error_token(src: &str, tok: &Token, info: &str) -> String {
    let mut err_msg = String::from("");
    err_msg.push_str(&format!("{}\n", src));
    let spaces = " ".repeat(tok.index);
    let arrows = "^".repeat(tok.len);
    err_msg.push_str(&format!("{}{} {}", spaces, arrows, info.red()));
    err_msg
}