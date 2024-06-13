 #![allow(warnings)]
use std::{io::{self, Write}, process::exit};
use std::rc::Rc;

#[derive(Clone)]
#[derive(PartialEq)]
enum TokenKind {
    TkPlus,
    TkMinus,
    TkMul,
    TkDiv,
    TkLParen,
    TkRParen,
    TkNum,   // Numeric literals
    TkEof,   // End-of-file markers
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    PlusMinus,
    MulDiv,
    Prefix,
}

#[derive(Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    val: i32,
    content: String,
    index: usize,
    len: usize,
}

impl Token {
    fn precedence(&self) -> Precedence {
        match self.kind {
            TokenKind::TkPlus | TokenKind::TkMinus => {
                return Precedence::PlusMinus;
            }
            TokenKind::TkMul | TokenKind::TkDiv => {
                return Precedence::MulDiv;
            }
            _ => {
                return Precedence::Lowest;
            }
        }
    }
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

// @Smell: we can wrap Expr enum in a struct with some meta data like locations
#[derive(PartialEq)]
enum ExprType {
    Number(i32),
    Binary(Box<Node>),
    Neg(Box<ExprType>),
}

use ExprType::*;

// binary expr
#[derive(PartialEq)]
struct Node {
    token: Token,
    lhs: ExprType,
    rhs: ExprType,
}

impl Node {
    fn new(token: Token, lhs: ExprType, rhs: ExprType) -> Self {
        Node {
            token,
            lhs,
            rhs,
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

    fn skip_peek(&mut self, target: &str) {
        let actual = self.peek_token().content.as_str();
        if  actual == target {
            self.cur_index += 1;
        } else {
            error_token(&self.src, self.peek_token(), format!("want '{}', but got '{}'", target, actual).as_str());
        }
    }

    fn skip(&mut self, target: &str) {
        let actual = self.cur_token().content.as_str();
        if actual == target {
            self.cur_index += 1;
        } else {
            error_token(&self.src, self.cur_token(), format!("want '{}', but got '{}'", target, actual).as_str());
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> ExprType {
        let mut expr: ExprType = self.parse_prefix();
        loop {
            if precedence >= self.peek_token().precedence() {
                return expr;
            }
            match self.peek_token().kind {
                TokenKind::TkPlus | TokenKind::TkMinus |
                TokenKind::TkMul | TokenKind::TkDiv => {
                    self.next_token();
                    expr = self.parse_infix(expr)
                },
                TokenKind::TkNum | TokenKind::TkLParen => {
                    error_token(&self.src, self.cur_token(), "expect an operator");
                },
                TokenKind::TkEof | TokenKind::TkRParen => {
                    break;
                },
            }
        }
        expr
    }

    fn parse_paren(&mut self) -> ExprType {
        self.skip("(");
        let res = self.parse_expr(Precedence::Lowest);
        self.skip_peek(")");
        res
    }

    fn parse_prefix(&mut self) -> ExprType {
        let mut neg_count = 0;
        if self.cur_token().kind == TokenKind::TkLParen {
            return self.parse_paren();
        }
        if self.cur_token().kind == TokenKind::TkNum {
            return self.parse_integer();
        }
        // prefix unary operators
        while (self.cur_token().kind == TokenKind::TkMinus) |
        (self.cur_token().kind == TokenKind::TkPlus) {
            if self.cur_token().kind == TokenKind::TkMinus {
                neg_count += 1;
            }
            self.next_token();
        }
        let expr = self.parse_expr(Precedence::Prefix);
        if neg_count % 2 == 0 {expr} else {Neg(Box::new(expr))}
    }

    fn parse_integer(&self) -> ExprType {
        if self.cur_token().kind != TokenKind::TkNum {
            error_token(&self.src, self.cur_token(), "expect a number");
        }
        Number(self.cur_token().val)
    }

    fn parse_infix(&mut self, lhs: ExprType) -> ExprType {
        let token = self.cur_token().clone();
        let p = token.precedence();
        self.next_token();
        let content = Node {
            token,
            lhs,
            rhs: self.parse_expr(p),
        };
        Binary(Box::new(content))
    }
}

fn main() {
    // test case
    let src = String::from("-10-1234*(2-3423)/11112");
    // let src = String::from("-3+2");
    println!("compiling source code: {src}");
    println!();

    let mut tokens: Vec<Token> = Vec::new();
    let tokens = tokenize(&src);
    let mut parser = Parser{
        src: src.clone(),
        tokens: tokens.clone(),
        cur_index: 0,
    };
    let program = parser.parse_expr(Precedence::Lowest);
    show_expr(&program);
    println!();

    println!("  .globl main");
    println!("main:");
    gen_code(program);
    println!("  ret");
}

fn show_expr(root: &ExprType) {
    match root {
        Number(n) => print!("{}", n),
        Binary(node) => {
            print!("(");
            show_expr(&node.lhs);
            print!("{}", node.token.content);
            show_expr(&node.rhs);
            print!(")");
        },
        Neg(expr) => {
            print!("-");
            show_expr(&*expr);
        },
    }
}

fn gen_code(expr: ExprType) {
    match expr {
        Number(n) => println!("  mov ${}, %rax", n),
        Binary(node) => {
            gen_code(node.rhs);
            println!("  push %rax");
            gen_code(node.lhs);
            println!("  pop %rdi");
            match node.token.kind {
                TokenKind::TkPlus => println!("  add %rdi, %rax"),
                TokenKind::TkMinus => println!("  sub %rdi, %rax"),
                TokenKind::TkMul => println!("  imul %rdi, %rax"),
                TokenKind::TkDiv => {
                    println!("  cqo");
                    println!("  idiv %rdi");
                },
                _ => println!("expect a binary operator"),
            }
        }
        Neg(expr) => {
            gen_code(*expr);
            println!("  neg %rax");
        },
    }
}

fn tokenize(src: &String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut index: usize = 0;
    while index < src.len() {
        let mut tok = Token::default();
        tok.index = index;
        if let Some(c) = src.chars().nth(index) {
            // @Cleanup: use switch case match
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
            } else if c == '*' {
                tok.kind = TokenKind::TkMul;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c == '/' {
                tok.kind = TokenKind::TkDiv;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c == '(' {
                tok.kind = TokenKind::TkLParen;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c == ')' {
                tok.kind = TokenKind::TkRParen;
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