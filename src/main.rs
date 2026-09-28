#![allow(warnings)]
pub mod analyze;
pub mod codegen;
pub mod ir;
pub mod lex;
pub mod parse;
pub mod preprocess;
// pub mod pretty;
pub mod common;
pub mod driver;
use crate::analyze::*;
use crate::codegen::*;
use crate::lex::*;
use crate::parse::*;
use crate::preprocess::*;
use crate::ExprType::*;
use crate::TokenKind::*;
use colored::*;
use std::sync::Mutex;
use std::{
    env, fs,
    io::{self, Read},
    process::exit,
};

static FILE_RECORDS: Mutex<Vec<Source_File>> = Mutex::new(Vec::new());

pub fn build_line_starts(src: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (i, c) in src.char_indices() {
        if c == '\n' {
            starts.push(i + 1);
        }
    }
    starts
}

fn compile(path: &str, output: Option<String>) -> Result<(), ()> {

    let file = load_file(path);
    let master_file_index = {
        let mut file_records = FILE_RECORDS.lock().unwrap();
        file_records.push(file);
        file_records.len() - 1
    };

    // lex
    let mut lexer = Lexer::new(master_file_index);
    let mut tokens = lexer.lex();

    // preprocess
    let mut preprocessed_tokens = preprocess(tokens);
    // parse
    let mut parser = Parser::new(preprocessed_tokens);
    let (program, syntax_errors) = parser.parse();
    if syntax_errors.is_empty() {
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
