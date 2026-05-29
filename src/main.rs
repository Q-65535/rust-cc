 #![allow(warnings)]
pub mod parse;
pub mod lex;
pub mod analyze;
pub mod codegen;
pub mod ir;
pub mod pretty;
pub mod common;
use std::{io::{self, Read, Write}, process::exit, env, fs};
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
    if args.len() <= 1 {
        eprintln!("{}: no input file", args[0]);
        exit(1);
    }

    let path = &args[1];
    let input = if path == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).expect("failed to read from stdin");
        buf
    } else {
        fs::read_to_string(path).unwrap_or_else(|err| {
            eprintln!("cannot open {}: {}", path, err);
            exit(1);
        })
    };
    {
        let mut src = SRC.lock().unwrap();
        *src = input.clone();
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
