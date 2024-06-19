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

static global_src: &str = "-10-1234*(2-3423)/11112;1+2;";



fn main() {
    // test case
    // let src = String::from("-10-1234*(2-3423)/11112");
    // let src = String::from("1=2");
    println!("compiling source code: '{global_src}'");
    let mut lexer = Lexer::new(global_src);

    let mut tokens: Vec<Token> = Vec::new();
    // let temp_tokens = tokenize(&src);
    // for c in temp_tokens {
    //     let a = c;
    // }
    let tokens = lexer.lex();
    let src = global_src.to_string().clone();
    let mut parser = parse::Parser::new(src, tokens, 0);
    // let res = parser.parse_expr(Precedence::Lowest);
    let res = parser.parse();
    match res {
        Ok(program) => {
            for stmt in program {
                match stmt {
                    StmtType::Ex(expr) => {
                        println!();
                        println!("assembly result:");
                        println!();
                        println!("  .globl main");
                        println!("main:");
                        gen_code(&expr);
                        println!("  ret");
                    }
                }
            }
            // show_expr(&program);
        },
        Err(err_msg) => {
            println!("{}", err_msg);
        },
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
                    let err_msg = error_token(global_src, &expr.token, "unknown binary token");
                    println!("{}", err_msg);
                },
            }
        }
        Neg(expr) => {
            gen_code(&expr);
            println!("  neg %rax");
        },
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