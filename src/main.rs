 #![allow(warnings)]
use std::{io::{self, Write}, process::exit, env};
use colored::*;
use crate::lex::*;
use crate::parse::*;
use crate::analyze::*;
use crate::TokenKind::*;
use crate::CompareToken::*;
use crate::ExprType::*;
use crate::codegen::*;
use std::sync::Mutex;
pub mod parse;
pub mod lex;
pub mod analyze;
pub mod codegen;

static SRC: Mutex<String> = Mutex::new(String::new());

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: String;
    if args.len() > 1 {
        input = args[1].clone();
        let mut src = SRC.lock().unwrap();
        *src = input.clone();
    } else {
        println!("No arguments provided.");
        return;
    }

    let src = SRC.lock().unwrap().clone();
    // lex
    let mut lexer = Lexer::new(&src);
    let mut tokens: Vec<Token> = Vec::new();
    let tokens = lexer.lex();
    // parse
    let mut parser = Parser::new(&src, tokens);
    match parser.parse() {
        Ok(program) => {
            // analyze
            let mut analyzer = Analyzer::new();
            let analyzed_program = analyzer.analyze(program);
            // codegen
            let mut gen = Generator::new(&src, analyzed_program);
            gen.gen_code();
        }
        Err(err) => {
            println!("{}", err);
            exit(0);
        }
    }
}
