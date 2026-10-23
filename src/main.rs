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

fn expand(input: &str, output: Option<String>) {
    // lex
    let mut tokens: Vec<Token>;
    {
        let file = load_file(input);
        let mut files = FILE_RECORDS.lock().unwrap();
        files.push(file);
        let master_file_index = files.len() - 1;
        let content_ref: &str = &files[master_file_index].content;
        let mut lexer = Lexer::new(master_file_index, content_ref);
        tokens = lexer.lex();
    }

    // preprocess
    let mut preprocessed_tokens = preprocess(tokens);
    let content = revert_tokens_to_string(preprocessed_tokens);

    if let Some(output) = output {
        if output == "-" {
            println!("{}", content);
        } else {
            use std::fs;
            fs::write(output, &content);
        }
    }
}

fn revert_tokens_to_string(tokens: Vec<Token>) -> String {
    let mut result = String::new();
    let source_files = &FILE_RECORDS.lock().unwrap();
    for tok in tokens {
        if tok.at_bol {
            result.push('\n');
        }
        let belonging_file = &source_files[tok.span.file_index];
        let src = &belonging_file.content;
        if tok.kind == Eof {
            continue;
        }
        let tok_content = &src[tok.span.start_index..tok.span.end_index+1];
        result.push_str(tok_content);
        result.push(' ');
    }
    return result;
}

fn compile(path: &str, output: Option<String>) -> Result<(), ()> {
    use std::time::Instant;
    let start = Instant::now();

    // lex
    let mut tokens: Vec<Token>;
    {
        let file = load_file(path);
        let mut files = FILE_RECORDS.lock().unwrap();
        files.push(file);
        let master_file_index = files.len() - 1;
        let content_ref: &str = &files[master_file_index].content;
        let mut lexer = Lexer::new(master_file_index, content_ref);
        tokens = lexer.lex();
    }

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
        let elapsed = start.elapsed();
        println!("Compile time: {:?}", elapsed);
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
            // If we manually run command like this: ./rust-cc -cc1 test.c,
            // then control flow goes here.
            path = options.inputs.first().unwrap().as_str();
        }
        let output = options.cc1_output.clone().or(options.output.clone());

        if options.preprocess_only {
            expand(path, output);
        } else {
            compile(path, output);
        }

    } else if let Err(err) = driver::run(&options, &args) {
        eprintln!("{err}");
        exit(1);
    }
}
