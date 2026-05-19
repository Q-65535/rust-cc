 #![allow(warnings)]
pub mod parse;
pub mod lex;
pub mod analyze;
pub mod codegen;
pub mod ir;
pub mod pretty;
pub mod common;
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
    }

    // lex
    let mut lexer: Lexer;
    {
        let src_str: &str = &SRC.lock().unwrap();
        lexer = Lexer::new(src_str);
    }
    let mut tokens = lexer.lex();
    // pretty::print_tokens(&tokens);
    // parse
    let mut parser = Parser::new(tokens);
    match parser.parse() {
        Ok(program) => {
            // pretty::print_program(&program);
            // analyze
            let mut analyzer = ProgramAnalyzer::new();
            let analyzed_program = analyzer.analyze(program);
            // codegen
            let mut gen = Generator::new(analyzed_program);
            gen.gen_code();
        }
        Err(err) => {
            println!("{}", err);
            exit(0);
        }
    }
}
