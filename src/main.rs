 #![allow(warnings)]
use std::{io::{self, Write}, process::exit};
use std::rc::Rc;

#[derive(Clone)]
#[derive(PartialEq)]
enum TokenKind {
    TkPlus,
    TkMinus,
    TkNum,   // Numeric literals
    TkEof,   // End-of-file markers
}

#[derive(Clone)]
struct Token {
    kind: TokenKind,
    val: i32,
    content: String,
    index: usize,
    len: usize,
}

impl Default for Token {
    fn default() -> Token {
        Token{
            kind: TokenKind::TkEof,
            val: 0,
            content: "".to_string(),
            index: 0,
            len: 0,
        }
    }
}

struct Parser {
    src: String,
    tokens: Vec<Token>,
    cur_index: usize,
}

impl Parser {
    fn cur_token(&self) -> &Token {
        &self.tokens[self.cur_index]
    }

    fn next_token(&mut self) {
        //@TODO check validity
        self.cur_index += 1;
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.cur_index+1]
    }
    fn skip(&mut self, target: &str) {
        if self.tokens[self.cur_index].content.as_str() == target {
            self.cur_index += 1;
        } else {
            error_token(&self.src, self.cur_token(), target)
        }
    }
}

fn main() {
    // test case
    let src = String::from("10+1234-2-3423+12312-1223412+11112");
    println!("compiling source code: {src}");

    let mut tokens: Vec<Token> = Vec::new();
    let tokens = tokenize(&src);
    let mut parser = Parser{
        src: src.clone(),
        tokens: tokens.clone(),
        cur_index: 0,
    };
    // special case: deal with the first token
    if let TokenKind::TkNum = parser.cur_token().kind {
        println!("mov ${} %rax", parser.cur_token().val);
        parser.next_token();
    } else {
        error_token(&src, parser.cur_token(), "expect a number for the first token");
    }
    while parser.cur_token().kind != TokenKind::TkEof {
        match parser.cur_token().kind {
            TokenKind::TkPlus => {
                if let TokenKind::TkNum = parser.peek_token().kind  {
                    println!("add ${}, %rax", parser.peek_token().val);
                    parser.next_token();
                    parser.next_token();
                } else {
                    error_token(&src, parser.peek_token(), "expect a number");
                }
            },
            TokenKind::TkMinus => {
                if let TokenKind::TkNum = parser.peek_token().kind  {
                    println!("sub ${}, %rax", parser.peek_token().val);
                    parser.next_token();
                    parser.next_token();
                } else {
                    error_token(&src, parser.peek_token(), "expect a number");
                }
            },
            TokenKind::TkNum => {
                error_token(&src, parser.cur_token(), "expect an operator");
            },
            TokenKind::TkEof => {
                error_token(&src, parser.cur_token(), "expect an operator");
            },
        }
    }
}

fn tokenize(src: &String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut index: usize = 0;
    while index < src.len() {
        let mut tok = Token::default();
        tok.index = index;
        if let Some(c) = src.chars().nth(index) {
            // @TODO: use switch case match
            if c == ' ' {
                index += 1;
                continue;
            } else if c == '+' {
                tok.kind = TokenKind::TkPlus;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1;
            } else if c == '-' {
                tok.kind = TokenKind::TkMinus;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c <= '9' && c >= '0' {
                let (len, int_str) = read_int(&src[index..]);
                tok.kind = TokenKind::TkNum;
                tok.val = int_str.parse().unwrap();;
                tok.content = int_str.to_string();
                tok.len = len;
                index += len; 
            } else {
                error_at(src, index, "invalid token");
            }
            tokens.push(tok);
        } else {
            println!("index out of bounds");
        }
    }
    let mut eof_tok = Token::default();
    eof_tok.kind = TokenKind::TkEof;
    tokens.push(eof_tok);
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

fn error_at(src: &String, index: usize, err_msg: &str) {
    println!("{}", src);
    let spaces = " ".repeat(index);
    println!("{}^ {}", spaces, err_msg);
    exit(0);
}

fn error_token(src: &String, tok: &Token, err_msg: &str) {
    println!("{}", src);
    let spaces = " ".repeat(tok.index);
    let arrows = "^".repeat(tok.len);
    println!("{}{} {}", spaces, arrows, err_msg);
    exit(0);
}