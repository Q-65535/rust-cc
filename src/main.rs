 #![allow(warnings)]
use std::{io::{self, Write}, process::exit};
use std::rc::Rc;
use colored::*;

static global_src: &str = "-10-1234*(2-3423)/11112";

#[derive(Clone)]
#[derive(PartialEq)]
enum TokenKind {
    Plus,
    Minus,
    Mul,
    Div,
    LParen,
    RParen,
    Assignment,
    Compare(CompareToken),
    Not,
    Num(i32),
    Eof,
}
use TokenKind::*;

#[derive(PartialEq, Clone)]
enum CompareToken {
    Eq,
    Neq,
    LT,
    LE,
}
use CompareToken::*;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Assign,
    Compare,
    PlusMinus,
    MulDiv,
    Prefix,
}

#[derive(Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    content: String,
    index: usize,
    len: usize,
}

impl Token {
    fn precedence(&self) -> Precedence {
        match self.kind {
            Plus | Minus => {
                return Precedence::PlusMinus;
            }
            Mul | Div => {
                return Precedence::MulDiv;
            }
            Compare(_) => {
                return Precedence::Compare;
            },
            Assignment => Precedence::Assign,
            _ => {
                return Precedence::Lowest;
            }
        }
    }
}

impl Default for Token {
    fn default() -> Token {
        Token{
            kind: Eof,
            content: "".to_string(),
            index: 0,
            len: 0,
        }
    }
}

struct Lexer {
    src: Vec<char>,
    index: usize,
}

impl Lexer {
    fn new(s: &str) -> Lexer {
        Lexer{
            src: s.chars().collect(),
            index: 0,
        }
    }

    fn cur_char(&self) -> char {
        self.src[self.index]
    }

    fn has_next(&self) -> bool {
        let len = self.src.len();
        self.index < len - 1
    }

    fn next_char(&mut self) {
        if self.index < self.src.len() {
            self.index += 1;
        } else {
            println!("lexing error: index {} out of bounds {}", self.index, self.src.len());
            exit(0);
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.index < self.src.len() - 1 {
            Some(self.src[self.index + 1])
        } else {
            None
        }
    }

    fn gen_token(kind: TokenKind, content: &str, index: usize, len: usize) -> Token {
        Token {
            kind,
            content: content.to_string(),
            index,
            len,
        }
    }

    fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let c = self.cur_char();
            let i = self.index;
            match c {
                ' ' => (),
                '+' => tokens.push(Self::gen_token(Plus, "+", i, 1)),
                '-' => tokens.push(Self::gen_token(Minus, "-", i, 1)),
                '*' => tokens.push(Self::gen_token(Mul, "*", i, 1)),
                '/' => tokens.push(Self::gen_token(Div, "/", i, 1)),
                '(' => tokens.push(Self::gen_token(LParen, "(", i, 1)),
                ')' => tokens.push(Self::gen_token(RParen, ")", i, 1)),
                '=' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(Eq), "==", i, 2));
                            self.next_char();
                        },
                        _ => {
                            tokens.push(Self::gen_token(Assignment, "=", i, 1));
                        },
                    }
                },
                '!' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(Neq), "!=", i, 2));
                            self.next_char();
                        },
                        _ => {
                            tokens.push(Self::gen_token(Not, "!", i, 1));
                        },
                    }
                },
                '<' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(LE), "<=", i, 2));
                            self.next_char();
                        },
                        _ => {
                            tokens.push(Self::gen_token(Compare(LT), "<", i, 1));
                        },
                    }
                },
                '0'..='9' => {
                    match self.read_int() {
                        Ok(num) => {
                            let num_str = &num.to_string();
                            tokens.push(Self::gen_token(Num(num), num_str, i, num_str.len()));
                        },
                        Err(s) => {
                            println!("{s}");
                            exit(0);
                        },
                    }
                },
                _ => (),
            }
            if !self.has_next() {
                break;
            }
            self.next_char();
        }
        tokens.push(Self::gen_token(Eof, "", 0, 0));
        tokens
    }

    fn read_int(&mut self) -> Result<i32, String> {
        let mut len: usize = 0;
        let c = self.cur_char(); 
        let i = self.index;
        if c <= '9' && c >= '0' {
            len += 1;
        } else {
            return Err("read_int() function error: the first char is not a digit".to_string());
        }
        loop {
            match self.peek_char() {
                Some(c) => {
                    if c <= '9' && c >= '0' {
                        len += 1;
                        self.next_char();
                    } else {
                        break;
                    }
                },
                None => {
                    break;
                }
            }
        }
        let s: &String = &self.src[i..i+len].iter().collect(); 
        Ok(s.parse().unwrap())
    }
}

#[derive(PartialEq)]
struct Expr {
    content: ExprType,
    token: Token,
}

impl Expr {
    fn new(content: ExprType, token: Token) -> Self {
        Expr{content, token}
    }
}

#[derive(PartialEq)]
enum ExprType {
    Number(i32),
    // Addition(Box<Expr>, Box<Expr>, TokenKind),
    Binary(Box<Expr>, Box<Expr>, TokenKind),
    Neg(Box<Expr>),
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
            let err_msg = error_token(&self.src, self.peek_token(), format!("want '{}', but got '{}'", target, actual).as_str());
            println!("{}", err_msg);
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

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, String> {
        match self.parse_prefix() {
            Err(err_msg) => return Err(err_msg),
            Ok(mut expr) => {
                loop {
                    if precedence >= self.peek_token().precedence() {
                        return Ok(expr);
                    }
                    match self.peek_token().kind {
                        Plus | Minus |
                        Mul | Div |
                        Compare(_) | Assignment => {
                            self.next_token();
                            expr = self.parse_infix(expr)?;
                        },
                        Num(_) | LParen => {
                            let err_msg = error_token(&self.src, self.cur_token(), "expect an operator");
                            return Err(err_msg);
                        },
                        Eof | RParen => {
                            break;
                        },
                        _ => {
                            let err_msg = error_token(&self.src, self.peek_token(), "not support parsing this token");
                            return Err(err_msg);
                        },
                    }
                }
                Ok(expr)
            }
        }
    }

    fn parse_paren(&mut self) -> Result<Expr, String> {
        self.skip("(");
        let res = self.parse_expr(Precedence::Lowest);
        self.skip_peek(")");
        res
    }

    fn parse_prefix(&mut self) -> Result<Expr, String> {
        let cur_token = self.cur_token().clone();
        match cur_token.kind {
            LParen => return self.parse_paren(),
            Num(_) => return self.parse_integer(),
            Plus => {
                self.next_token();
                return self.parse_prefix();
            },
            Minus => {
                self.next_token();
                let prefix = self.parse_prefix()?;
                let operand = Box::new(prefix);
                let expr = Expr::new(Neg(operand), cur_token);
                return Ok(expr);
            },
            _ => {
                let err_msg = error_token(&self.src, &cur_token, "can't parse prefix expression here");
                return Err(err_msg);
            }
        }
    }

    fn parse_integer(&self) -> Result<Expr, String> {
        let cur_token = self.cur_token();
        if let Num(n) = cur_token.kind {
            let expr = Expr::new(Number(n), cur_token.clone());
            return Ok(expr);
        } else {
            return Err(error_token(&self.src, self.cur_token(), "expect a number"));
        }
    }

    fn parse_infix(&mut self, lhs: Expr) -> Result<Expr, String> {
        let token = self.cur_token().clone();
        let p = token.precedence();
        self.next_token();
        let rhs = self.parse_expr(p)?;
        let content = Binary(Box::new(lhs), Box::new(rhs), token.kind.clone());
        Ok(Expr::new(content, token))
    }
}

fn main() {
    // test case
    // let src = String::from("-10-1234*(2-3423)/11112");
    // let src = String::from("1=2");
    println!("compiling source code: '{global_src}'");
    let mut lexer = Lexer::new(global_src);

    let mut tokens: Vec<Token> = Vec::new();
    // let temp_tokens = tokenize(&src);
    // for c in temp_tokens {
    //     let a = c;
    // }
    let tokens = lexer.lex();
    let mut parser = Parser{
        src: global_src.to_string().clone(),
        tokens: tokens.clone(),
        cur_index: 0,
    };
    let res = parser.parse_expr(Precedence::Lowest);
    match res {
        Ok(program) => {
            show_expr(&program);
            println!();
            println!("assembly result:");
            println!();
            println!("  .globl main");
            println!("main:");
            gen_code(&program);
            println!("  ret");
        },
        Err(err_msg) => {
            println!("{}", err_msg);
        },
    }
    
}

fn show_expr(root: &Expr) {
    match &root.content {
        Number(n) => print!("{}", n),
        Binary(lhs, rhs, kind) => {
            print!("(");
            show_expr(&lhs);
            print!("{}", root.token.content);
            show_expr(&rhs);
            print!(")");
        },
        Neg(expr) => {
            print!("-");
            show_expr(&expr);
        },
    }
}

fn gen_code(expr: &Expr) {
    let content = &expr.content;
    match content {
        Number(n) => println!("  mov ${}, %rax", n),
        Binary(lhs, rhs, kind) => {
            gen_code(&rhs);
            println!("  push %rax");
            gen_code(&lhs);
            println!("  pop %rdi");
            match kind  {
                Plus => println!("  add %rdi, %rax"),
                Minus => println!("  sub %rdi, %rax"),
                Mul => println!("  imul %rdi, %rax"),
                Div => {
                    println!("  cqo");
                    println!("  idiv %rdi");
                },
                Compare(c) => {
                    println!("  cmp %rdi, %rax");
                    match c {
                        Eq => println!("  sete %al"),
                        Neq => println!("  setne %al"),
                        LT => println!("  selt %al"),
                        LE => println!("  sele %al"),
                    }
                    println!("  movzb %al, %rax");
                },
                _ => {
                    let err_msg = error_token(global_src, &expr.token, "unknown binary token");
                    println!("{}", err_msg);
                },
            }
        }
        Neg(expr) => {
            gen_code(&expr);
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
            match c {
                ' ' => index += 1,
                '+' => {
                    // (Plus, content, len, index)
                    tok.kind = Plus;
                    tok.content = String::from(c);
                    tok.len = 1;
                    index += 1;
                },
                _ => (),
            }
            if c == ' ' {
                index += 1;
                continue;
            } else if c == '+' {
                tok.kind = Plus;
            } else if c == '-' {
                tok.kind = Minus;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c == '*' {
                tok.kind = Mul;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c == '/' {
                tok.kind = Div;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c == '(' {
                tok.kind = LParen;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c == ')' {
                tok.kind = RParen;
                tok.content = String::from(c);
                tok.len = 1;
                index += 1; 
            } else if c <= '9' && c >= '0' {
                let (len, int_str) = read_int(&src[index..]);
                tok.kind = Num(int_str.parse().unwrap());
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
    eof_tok.kind = Eof;
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
    println!("{}^ {}", spaces, err_msg.red());
    exit(0);
}

fn error_token(src: &str, tok: &Token, info: &str) -> String {
    let mut err_msg = String::from("");
    err_msg.push_str(&format!("{}\n", src));
    let spaces = " ".repeat(tok.index);
    let arrows = "^".repeat(tok.len);
    err_msg.push_str(&format!("{}{} {}", spaces, arrows, info.red()));
    err_msg
}