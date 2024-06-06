 #![allow(warnings)]
use std::io::{self, Write};

enum TokenKind {
    TkPlus,
    TkMinus,
    TkNum,   // Numeric literals
    TkEof,   // End-of-file markers
}

struct Token {
    kind: TokenKind,
    val: i32,
    content: String,
    index: usize,
    len: usize,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::TkEof,
            val: 0,
            content: "default str".to_string(),
            index: 0,
            len: 0,
        }
    }
}

fn main() {
    // test case
    let src = String::from("100+99-10+123455-23423+12312-1223412+11112");

    let mut tokens: Vec<Token> = Vec::new();
    let tokens = tokenize(&src);
    if let TokenKind::TkNum = tokens[0].kind {
        println!("mov ${} %rax", tokens[0].val);
    } else {
        println!("err");
    }
    let mut i = 1;
    while i < tokens.len() {
        match tokens[i].kind {
            TokenKind::TkPlus => {
                println!("add ${}, %rax", tokens[i+1].val);
                i += 2;
            },
            TokenKind::TkMinus => {
                println!("sub ${}, %rax", tokens[i+1].val);
                i += 2;
            },
            TokenKind::TkNum => {
                i += 1;
                println!("lexing error: current token should not be {}", tokens[i].content);
            },
            TokenKind::TkEof => {
                i += 1;
                println!("lexing error: current token should not be {}", tokens[i].content);
            },
        }
    }
}

fn tokenize(src: &String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut index: usize = 0;
    while index < src.len() {
        let mut tok = Token::default();
        if let Some(c) = src.chars().nth(index) {
            if c== '+' {
                tok.kind = TokenKind::TkPlus;
                tok.content = String::from(c);
                tok.index = index;
                tok.len = 1;
                index += 1;
            } else if c == '-' {
                tok.kind = TokenKind::TkMinus;
                tok.content = String::from(c);
                tok.index = index;
                tok.len = 1;
                index += 1; 
            } else if c <= '9' && c >= '0' {
                let (len, int_str) = read_int(&src[index..]);
                tok.kind = TokenKind::TkNum;
                tok.val = int_str.parse().unwrap();;
                tok.content = int_str.to_string();
                tok.index = index;
                tok.len = len;
                index += len; 
            }
            tokens.push(tok);
        } else {
            println!("index out of bounds");
        }
    }
    tokens
}

fn read_int(src: &str) -> (usize, &str) {
    let mut len: usize = 0;
    for c in src.chars() {
        if c <= '9' && c >= '0' {
            len += 1;
        } else {
            break;
        }
    }
    (len, &src[..len])
}