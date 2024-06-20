use std::{io::{self, Write}, process::exit};
use crate::*;

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
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
    Semicolon,
    Eof,
}
use TokenKind::*;

#[derive(PartialEq, Clone, Debug)]
pub enum CompareToken {
    Eq,
    Neq,
    LT,
    LE,
}
use CompareToken::*;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,
    Comparison,
    PlusMinus,
    MulDiv,
    Prefix,
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub content: String,
    pub index: usize,
    pub len: usize,
}

impl Token {
    pub fn precedence(&self) -> Precedence {
        match self.kind {
            Plus | Minus => {
                return Precedence::PlusMinus;
            }
            Mul | Div => {
                return Precedence::MulDiv;
            }
            Compare(_) => {
                return Precedence::Comparison;
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

pub struct Lexer {
    src: Vec<char>,
    index: usize,
}

impl Lexer {
    pub fn new(s: &str) -> Lexer {
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

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let c = self.cur_char();
            let i = self.index;
            match c {
                ' ' => (),
                ';' => tokens.push(Self::gen_token(Semicolon, ";", i, 1)),
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
                _ => {
                    println!("lexing error: unknown character: '{}'", c);
                    exit(0);
                },
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