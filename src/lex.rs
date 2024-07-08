use std::{io::{self, Write}, process::exit};
use std::collections::HashMap;
use crate::*;

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    Plus,
    Minus,
    Mul,
    Div,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Assignment,
    Compare(CompareToken),
    Not,
    Num(i32),
    Semicolon,
    Ident(String),
    Keyword(KeywordToken),
    Eof,
}
use TokenKind::*;

#[derive(PartialEq, Clone, Debug)]
pub enum CompareToken {
    Eq,
    Neq,
    LT,
    LE,
    GT,
    GE,
}
use CompareToken::*;

#[derive(PartialEq, Clone, Debug)]
pub enum KeywordToken {
    Ret,
    If,
    Else,
}
use KeywordToken::*;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,
    Comparison,
    PlusMinus,
    MulDiv,
    Prefix,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
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
            start: 0,
            end: 0,
        }
    }
}

pub struct Lexer {
    src: Vec<char>,
    index: usize,
    keywords: HashMap<String, TokenKind>,
}

impl Lexer {
    pub fn new(s: &str) -> Lexer {
        let keywords: HashMap<String, TokenKind> = vec![
            // add more keywords here 
            ("return".to_string(), Keyword(Ret)),
            ("if".to_string(), Keyword(If)),
            ("else".to_string(), Keyword(Else)),
        ].into_iter().collect();
        Lexer{
            src: s.chars().collect(),
            index: 0,
            keywords,
        }
    }

    fn is_keyword(&self, name: &str) -> bool {
        self.keywords.contains_key(name)
    }

    fn get_tok_kind(&self, name: &str) -> TokenKind {
        self.keywords.get(name).unwrap().clone()
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

    fn gen_token(kind: TokenKind, content: &str, start: usize, size: usize) -> Token {
        Token {
            kind,
            start,
            end: start+size,
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
                '{' => tokens.push(Self::gen_token(LBrace, "{", i, 1)),
                '}' => tokens.push(Self::gen_token(RBrace, "}", i, 1)),
                'a'..='z' | '_' => {
                    let name = self.read_ident();
                    let tok_kind = if self.is_keyword(&name) {
                        self.get_tok_kind(&name)
                    } else {
                        Ident(name.clone())
                    };
                    let tok = Self::gen_token(tok_kind, &name, i, name.len());
                    tokens.push(tok);
                },
                '=' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(Eq), "==", i, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Assignment, "=", i, 1)),
                    }
                },
                '!' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(Neq), "!=", i, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Not, "!", i, 1)),
                    }
                },
                '<' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(LE), "<=", i, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Compare(LT), "<", i, 1)),
                    }
                },
                '>' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(GE), ">=", i, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Compare(GT), ">", i, 1)),
                    }
                },
                '0'..='9' => {
                    match self.read_int() {
                        Ok(num) => {
                            let num_str = &num.to_string();
                            tokens.push(Self::gen_token(Num(num), num_str, i, num_str.len()));
                        },
                        Err(s) => {
                            self.error_at(i, "unable to read integer.");
                            exit(0);
                        },
                    }
                },
                _ => {
                    let err_msg = &format!("lexing error: unknown character: '{}'", c);
                    self.error_at(i, err_msg);
                    exit(0);
                },
            }
            if !self.has_next() {
                break;
            }
            self.next_char();
        }
        tokens.push(Self::gen_token(Eof, "", self.src.len(), 1));
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

    fn read_ident(&mut self) -> String {
        let mut len = 1;
        let i = self.index;
        loop {
            match self.peek_char() {
                Some(c) => {
                    if (c <= '9' && c >= '0') | (c >= 'a' && c <= 'z') | (c == '_') {
                        len += 1;
                        self.next_char();
                    } else {
                        break;
                    }
                },
                None => break,
            }
        }
        self.src[i..i+len].iter().collect()
    }

    fn error_at(&self, index: usize, err_msg: &str) {
        let src_str: &String = &self.src[0..self.src.len()].iter().collect(); 
        println!("{}", src_str);
        let spaces = " ".repeat(index);
        println!("{}^ {}", spaces, err_msg.red());
        exit(0);
    }
}