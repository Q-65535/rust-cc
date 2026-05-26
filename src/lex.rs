use std::{io::{self, Write}, process::exit};
use std::cmp::{max, min};
use std::collections::HashMap;
use crate::*;
use crate::SRC;
use crate::common::{self, *};

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
    LSqureBracket,
    RSqureBracket,
    Assignment,
    Compare(CompareToken),
    Not,
    Ampersand,
    Num(i32),
    Semicolon,
    Comma,
    LexIdent(String),
    StringLiteral(String),
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
    For,
    While,
    Sizeof,
    TypeSpecifier(TypeSpecifier),
}
use KeywordToken::*;

#[derive(PartialEq, Clone, Debug)]
pub enum TypeSpecifier {
    Int,
    Char,
}
use TypeSpecifier::*;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,
    Comparison,
    PlusMinus,
    MulDiv,
    Funcall,
    Prefix,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub val: String,
    pub span: Span,
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
            LParen | LSqureBracket => Precedence::Funcall,
            _ => {
                return Precedence::Lowest;
            }
        }
    }
}

impl Default for Token {
    fn default() -> Token {
        let default_location = Span{start_index: 0, end_index: 0};
        Token{
            kind: Eof,
            val: "".to_string(),
            span: default_location,
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
            ("for".to_string(), Keyword(For)),
            ("while".to_string(), Keyword(While)),
            ("int".to_string(), Keyword(TypeSpecifier(Int))),
            ("sizeof".to_string(), Keyword(KeywordToken::Sizeof)),
            ("char".to_string(), Keyword(TypeSpecifier(Char))),
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

    pub fn gen_token(kind: TokenKind, content: &str, start_index: usize, len: usize) -> Token {
        if !(start_index+len >= 1) {
            println!("fatal: this token {} has length of 0", content);
        }
        let span = Span{start_index, end_index: start_index+len-1};
        Token {
            kind,
            val: content.to_string(),
            span,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let c = self.cur_char();
            let start_index = self.index;
            match c {
                ' ' => (),
                ';' => tokens.push(Self::gen_token(Semicolon, ";", start_index, 1)),
                ',' => tokens.push(Self::gen_token(Comma, ",", start_index, 1)),
                '+' => tokens.push(Self::gen_token(Plus, "+", start_index, 1)),
                '-' => tokens.push(Self::gen_token(Minus, "-", start_index, 1)),
                '*' => tokens.push(Self::gen_token(Mul, "*", start_index, 1)),
                '/' => tokens.push(Self::gen_token(Div, "/", start_index, 1)),
                '(' => tokens.push(Self::gen_token(LParen, "(", start_index, 1)),
                ')' => tokens.push(Self::gen_token(RParen, ")", start_index, 1)),
                '{' => tokens.push(Self::gen_token(LBrace, "{", start_index, 1)),
                '}' => tokens.push(Self::gen_token(RBrace, "}", start_index, 1)),
                '[' => tokens.push(Self::gen_token(LSqureBracket, "[", start_index, 1)),
                ']' => tokens.push(Self::gen_token(RSqureBracket, "]", start_index, 1)),
                '&' => tokens.push(Self::gen_token(Ampersand, "&", start_index, 1)),
                'a'..='z' | '_' => {
                    let name = self.read_ident();
                    let tok_kind = if self.is_keyword(&name) {
                        self.get_tok_kind(&name)
                    } else {
                        LexIdent(name.clone())
                    };
                    let tok = Self::gen_token(tok_kind, &name, start_index, name.len());
                    tokens.push(tok);
                },
                '=' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(Eq), "==", start_index, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Assignment, "=", start_index, 1)),
                    }
                },
                '!' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(Neq), "!=", start_index, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Not, "!", start_index, 1)),
                    }
                },
                '<' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(LE), "<=", start_index, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Compare(LT), "<", start_index, 1)),
                    }
                },
                '>' => {
                    match self.peek_char() {
                        Some('=') => {
                            tokens.push(Self::gen_token(Compare(GE), ">=", start_index, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Compare(GT), ">", start_index, 1)),
                    }
                },
                '0'..='9' => {
                    match self.read_int() {
                        Ok(num) => {
                            let num_str = &num.to_string();
                            tokens.push(Self::gen_token(Num(num), num_str, start_index, num_str.len()));
                        },
                        Err(s) => {
                            self.error_at(start_index, "unable to read integer.");
                            exit(0);
                        },
                    }
                },
                '"' => {
                    match self.read_string() {
                        Ok(s) => {
                            tokens.push(Self::gen_token(StringLiteral(s.clone()), &s, start_index, s.len()+2));
                        },
                        Err(s) => {
                            self.error_at(start_index, "unable to read string literal.");
                            exit(0);
                        },
                    }
                },
                _ => {
                    let err_msg = &format!("lexing error: unknown character: '{}'", c);
                    self.error_at(start_index, err_msg);
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

    fn read_string(&mut self) -> Result<String, String> {
        let mut s = String::new();
        // This len does not include " at the begining and end of a string.
        self.next_char(); // skip " character
        let start_index = self.index;
        while self.cur_char() != '"' {
            let current_char: char;
            if self.cur_char() == '\\' {
                current_char = self.read_escaped_char();
            } else {
                current_char = self.cur_char();
            }
            s.push(current_char);
            self.next_char();
        }
        Ok(s)
    }

    fn read_escaped_char(&mut self) -> char {
        self.next_char(); // skip '\' character
        let mut c = self.cur_char();
        if c >= '0' && c <= '7' {
            return self.read_escaped_octal();
        }
        match self.cur_char() {
            'a' => '\x07',
            'b' => '\x08',
            't' => '\t',
            'n' => '\n',
            'v' => '\x0B',
            'f' => '\x0C',
            'r' => '\r',
            'e' => '\x1B',
            _ => self.cur_char(),
        }
    }

    fn read_escaped_octal(&mut self) -> char {
        let mut num: u8 = 0;
        // A char is at most 255 (8 bytes), 3-digit octal number can cover a char value range.
        // So the for loop iterate at most 3 times.
        for i in 0..3 {
            let digit_number = self.cur_char() as u8 - b'0';
            num <<= 3;
            num += digit_number;
            if self.peek_char() >= Some('0') && self.peek_char() <= Some('7') {
                self.next_char();
            } else {
                break;
            }
        }
        return num as char;
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
        let src_str: &str = &SRC.lock().unwrap().to_string();
        println!("{}", src_str);
        let spaces = " ".repeat(index);
        println!("{}^ {}", spaces, err_msg.red());
        exit(0);
    }
}