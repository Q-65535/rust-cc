use std::{io::{self, Write}, process::exit};
use std::cmp::{max, min};
use std::collections::HashMap;
use crate::*;
use crate::SRC;
use crate::common::{self, *};

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    Period,
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
    Arrow,
    Ampersand,
    Num(i32),
    Semicolon,
    Comma,
    LexIdent(String),
    StringLiteral(Vec<u8>),
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
    Struct,
    TypeSpecifier(TypeSpecifier),
}
use KeywordToken::*;

#[derive(PartialEq, Clone, Debug)]
pub enum TypeSpecifier {
    Int,
    Char,
}
use TypeSpecifier::*;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    LV1,
    LV2,
    LV3,
    LV4,
    LV5,
    LV6,
    LV7,
}

impl std::ops::Sub<u8> for Precedence {
    type Output = Precedence;
    fn sub(self, rhs: u8) -> Precedence {
        match (self as u8).saturating_sub(rhs) {
            0 => Precedence::Lowest,
            1 => Precedence::LV1,
            2 => Precedence::LV2,
            3 => Precedence::LV3,
            4 => Precedence::LV4,
            5 => Precedence::LV5,
            6 => Precedence::LV6,
            7 => Precedence::LV7,
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub val: String,
    pub span: Span,
}

impl Token {
    pub fn precedence(&self) -> Precedence {
        return precedence(&self.kind);
    }
}

pub fn precedence(kind: &TokenKind) -> Precedence {
    return match kind {
        Comma => Precedence::LV1,
        Assignment => Precedence::LV2,
        Compare(_) => Precedence::LV3,
        Plus | Minus => Precedence::LV4,
        Mul | Div => Precedence::LV5,
        LParen | LSqureBracket => Precedence::LV6,
        Period | Arrow => Precedence::LV7,
        _ => Precedence::Lowest,
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
            // Add more keywords here.
            ("return".to_string(), Keyword(Ret)),
            ("if".to_string(), Keyword(If)),
            ("else".to_string(), Keyword(Else)),
            ("for".to_string(), Keyword(For)),
            ("while".to_string(), Keyword(While)),
            ("int".to_string(), Keyword(TypeSpecifier(Int))),
            ("sizeof".to_string(), Keyword(KeywordToken::Sizeof)),
            ("struct".to_string(), Keyword(KeywordToken::Struct)),
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

    fn get_keyword_kind(&self, name: &str) -> TokenKind {
        self.keywords.get(name).unwrap().clone()
    }

    fn cur_char(&self) -> char {
        self.src[self.index]
    }

    fn has_next(&self) -> bool {
        let len = self.src.len();
        self.index + 1 < len
    }

    fn next_char(&mut self) {
        if self.has_next() {
            self.index += 1;
        } else {
            let error_message = format!("lexing error: index {} out of bounds {}", self.index, self.src.len());
            lex_error_at(self.index, &error_message);
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
        let span = Span{start_index, end_index: start_index+len-1};
        Token {
            kind,
            val: content.to_string(),
            span,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        if self.src.is_empty() {
            tokens.push(Self::gen_token(Eof, "", self.src.len(), 1));
            return tokens;
        }
        loop {
            let c = self.cur_char();
            let start_index = self.index;
            match c {
                ' ' | '\t' | '\n' | '\r' => (),
                '.' => tokens.push(Self::gen_token(Period, ".", start_index, 1)),
                ';' => tokens.push(Self::gen_token(Semicolon, ";", start_index, 1)),
                ',' => tokens.push(Self::gen_token(Comma, ",", start_index, 1)),
                '+' => tokens.push(Self::gen_token(Plus, "+", start_index, 1)),
                '(' => tokens.push(Self::gen_token(LParen, "(", start_index, 1)),
                ')' => tokens.push(Self::gen_token(RParen, ")", start_index, 1)),
                '{' => tokens.push(Self::gen_token(LBrace, "{", start_index, 1)),
                '}' => tokens.push(Self::gen_token(RBrace, "}", start_index, 1)),
                '[' => tokens.push(Self::gen_token(LSqureBracket, "[", start_index, 1)),
                ']' => tokens.push(Self::gen_token(RSqureBracket, "]", start_index, 1)),
                '&' => tokens.push(Self::gen_token(Ampersand, "&", start_index, 1)),
                '-' => {
                    match self.peek_char() {
                        Some('>') => {
                            tokens.push(Self::gen_token(Arrow, "->", start_index, 2));
                            self.next_char();
                        },
                        _ => tokens.push(Self::gen_token(Minus, ">", start_index, 1)),
                    }
                },
                '*' => tokens.push(Self::gen_token(Mul, "*", start_index, 1)),
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
                        self.get_keyword_kind(&name)
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
                    let num = self.read_int();
                    let num_str = num.to_string();
                    tokens.push(Self::gen_token(Num(num), &num_str, start_index, num_str.len()));
                },
                '"' => {
                    match self.read_string() {
                        Ok(bytes) => {
                            let display = String::from_utf8_lossy(&bytes).into_owned();
                            let consumed = self.index - start_index + 1;
                            tokens.push(Self::gen_token(StringLiteral(bytes), &display, start_index, consumed));
                        },
                        Err(s) => {
                            lex_error_at(start_index, &s);
                        },
                    }
                },
                '/' => {
                    match self.peek_char() {
                        Some('/') => {
                            // Line comment: skip until end of line.
                            self.next_char();
                            while let Some(nc) = self.peek_char() {
                                if nc == '\n' { break; }
                                self.next_char();
                            }
                        },
                        Some('*') => {
                            // Block comment: skip until closing "*/".
                            self.next_char();
                            loop {
                                match self.peek_char() {
                                    Some('*') => {
                                        self.next_char();
                                        if let Some('/') = self.peek_char() {
                                            self.next_char();
                                            break;
                                        }
                                    },
                                    Some(_) => self.next_char(),
                                    None => {
                                        lex_error_at(start_index, "unclosed block comment");
                                    },
                                }
                            }
                        },
                        _ => tokens.push(Self::gen_token(Div, "/", start_index, 1)),
                    }
                },
                _ => {
                    let err_msg = format!("lexing error: unknown character: '{}'", c);
                    lex_error_at(start_index, &err_msg);
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

    fn read_int(&mut self) -> i32 {
        debug_assert!(matches!(self.cur_char(), '0'..='9'));
        let c = self.cur_char();
        let mut len: usize = 1;
        let start_index = self.index;
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
        let integer_string: &String = &self.src[start_index..start_index+len].iter().collect(); 
        return integer_string.parse().unwrap();
    }

    fn read_string(&mut self) -> Result<Vec<u8>, String> {
        debug_assert!(matches!(self.cur_char(), '"'));
        let mut bytes: Vec<u8> = Vec::new();
        loop {
            if self.has_next() {
                self.next_char();
            } else {
                let error_info = "reaching end of file without seeing closing \" while parsing string literal".to_string();
                return Err(error_info);
            }
            if self.cur_char() == '"' {
                return Ok(bytes);
            }
            if self.cur_char() == '\\' {
                bytes.push(self.read_escaped_char());
            } else {
                let mut buf = [0u8; 4];
                let encoded = self.cur_char().encode_utf8(&mut buf);
                bytes.extend_from_slice(encoded.as_bytes());
            }
        }
    }

    fn read_escaped_char(&mut self) -> u8 {
        self.next_char(); // skip '\' character
        let c = self.cur_char();
        if c >= '0' && c <= '7' {
            return self.read_escaped_octal();
        } else if c == 'x' {
            return self.read_escaped_hex();
        }
        match c {
            'a' => 0x07,
            'b' => 0x08,
            't' => b'\t',
            'n' => b'\n',
            'v' => 0x0B,
            'f' => 0x0C,
            'r' => b'\r',
            'e' => 0x1B,
            _ => c as u8,
        }
    }

    fn read_escaped_octal(&mut self) -> u8 {
        let mut num: u8 = 0;
        // A char is at most 255 (8 bits), so a 3-digit octal number suffices.
        for _ in 0..3 {
            let digit_number = self.cur_char() as u8 - b'0';
            num = num.wrapping_shl(3).wrapping_add(digit_number);
            if self.peek_char() >= Some('0') && self.peek_char() <= Some('7') {
                self.next_char();
            } else {
                break;
            }
        }
        num
    }

    fn read_escaped_hex(&mut self) -> u8 {
        self.next_char(); // Skip 'x' character.
        let mut num: u8 = 0;
        let mut c = self.cur_char();
        if !c.is_ascii_hexdigit() {
            println!("lex hexdigit error, wrong character following \\x");
            exit(0);
        }
        while c.is_ascii_hexdigit() {
            let digit_number;
            if c >= '0' && c <= '9' {
                digit_number = c as u8 - b'0';
            } else if c >= 'A' && c <= 'F' {
                digit_number = c as u8 - b'A' + 10;
            } else {
                digit_number = c as u8 - b'a' + 10;
            }
            // Per the C standard, \x consumes an unbounded run of hex digits;
            // overflow past one byte is allowed and the low 8 bits are kept.
            num = num.wrapping_shl(4).wrapping_add(digit_number);
            if self.peek_char().expect("no char left to be peeked").is_ascii_hexdigit() {
                self.next_char();
                c = self.cur_char();
            } else {
                break;
            }
        }
        num
    }

    fn read_ident(&mut self) -> String {
        debug_assert!(matches!(self.cur_char(), 'a'..='z' | '_'));
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

}

fn lex_error_at(index: usize, err_msg: &str) {
    let span = Span{start_index: index, end_index: index};
    report_error_at(span, err_msg);
    // Lex error is strict, once encountered, we force compilation to stop.
    exit(1);
}

// @Duplication: Duplicate with error reporter in parse.rs.
pub fn report_error_at(span: Span, info: &str) {
    let (start_line, satart_column, end_line, end_column) = span.locate();
    // @Incomplete: Handle error across multiple lines.
    let start_line_content = get_src_content_at_line(start_line);
    let mut err_msg = String::new();
    err_msg.push_str(&start_line_content);
    err_msg.push_str("\n");
    let spaces = " ".repeat(satart_column - 1);
    let arrows = if start_line == end_line {
        "^".repeat(span.end_index - span.start_index + 1)
    } else {
        "^".to_string()
    };
    err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
    println!("{}", err_msg);
}