#![allow(warnings)]
pub mod analyze;
pub mod codegen;
pub mod ir;
pub mod lex;
pub mod parse;
// pub mod pretty;
pub mod common;
pub mod driver;
use crate::analyze::*;
use crate::codegen::*;
use crate::lex::*;
use crate::parse::*;
use crate::ExprType::*;
use crate::TokenKind::*;
use colored::*;
use std::sync::Mutex;
use std::{
    env, fs,
    io::{self, Read},
    process::exit,
};

static SRC: Mutex<String> = Mutex::new(String::new());
static INPUT_PATH: Mutex<String> = Mutex::new(String::new());
static LINE_STARTS: Mutex<Vec<usize>> = Mutex::new(Vec::new());

fn build_line_starts(src: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (i, c) in src.chars().enumerate() {
        if c == '\n' {
            starts.push(i + 1);
        }
    }
    starts
}

fn compile(path: &str, output: Option<String>) -> Result<(), ()> {
    *INPUT_PATH.lock().unwrap() = path.to_string();

    let input = if path == "-" {
        let mut buf = String::new();
        io::stdin()
            .read_to_string(&mut buf)
            .expect("failed to read from stdin");
        buf
    } else {
        fs::read_to_string(&path).unwrap_or_else(|err| {
            eprintln!("cannot open {}: {}", path, err);
            exit(1);
        })
    };
    {
        let mut src = SRC.lock().unwrap();
        *src = input.clone();
    }
    {
        let mut line_starts = LINE_STARTS.lock().unwrap();
        *line_starts = build_line_starts(&input);
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
    let (program, syntax_errors) = parser.parse();
    if syntax_errors.is_empty() {
        // pretty::print_program(&program);
        // analyze
        let mut analyzer = ProgramAnalyzer::new();
        let analyzed_program = analyzer.analyze(program);
        // codegen
        set_output(&output);
        let mut gen = Generator::new();
        gen.gen_code(analyzed_program);
        Ok(())
    } else {
        for e in syntax_errors {
            eprintln!("{}", e);
        }
        Err(())
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let options = driver::parse_args(&args).unwrap_or_else(|err| {
        eprintln!("{err}");
        driver::usage(&args[0]);
        exit(1);
    });

    if options.cc1 {
        let path: &str;
        if let Some(input) = options.cc1_input.as_deref() {
            path = input;
        } else {
            path = options.inputs.first().unwrap().as_str();
        }
        let output = options.cc1_output.clone().or(options.output.clone());
        if compile(path, output).is_err() {
            exit(1);
        }
    } else if let Err(err) = driver::run(&options, &args) {
        eprintln!("{err}");
        exit(1);
    }
}
