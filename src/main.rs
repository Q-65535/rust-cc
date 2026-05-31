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
static LINE_STARTS: Mutex<Vec<usize>> = Mutex::new(Vec::new());

fn usage(status: i32) -> ! {
    eprintln!("rust-cc [ -o <path> ] <file>");
    exit(status);
}

fn parse_args(args: &[String]) -> (Option<String>, String) {
    let mut opt_o: Option<String> = None;
    let mut input_path: Option<String> = None;

    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];

        if arg == "--help" {
            usage(0);
        }

        if arg == "-o" {
            i += 1;
            if i >= args.len() {
                usage(1);
            }
            opt_o = Some(args[i].clone());
            i += 1;
            continue;
        }

        if let Some(rest) = arg.strip_prefix("-o") {
            opt_o = Some(rest.to_string());
            i += 1;
            continue;
        }

        if arg.starts_with('-') && arg.len() > 1 {
            eprintln!("unknown argument: {}", arg);
            exit(1);
        }

        input_path = Some(arg.clone());
        i += 1;
    }

    match input_path {
        Some(path) => (opt_o, path),
        None => {
            eprintln!("no input files");
            exit(1);
        }
    }
}

fn build_line_starts(src: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (i, c) in src.chars().enumerate() {
        if c == '\n' {
            starts.push(i + 1);
        }
    }
    starts
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let (opt_o, path) = parse_args(&args);

    let input = if path == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).expect("failed to read from stdin");
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
    match parser.parse() {
        Ok(program) => {
            // pretty::print_program(&program);
            // analyze
            let mut analyzer = ProgramAnalyzer::new();
            let analyzed_program = analyzer.analyze(program);
            // codegen
            set_output(&opt_o);
            let mut gen = Generator::new(analyzed_program);
            gen.gen_code();
        }
        Err(err) => {
            println!("{}", err);
            exit(0);
        }
    }
}
