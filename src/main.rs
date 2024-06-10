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
    TkNum,   // Numeric literals
    TkEof,   // End-of-file markers
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    PlusMinus,
    MulDiv,
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

#[derive(PartialEq)]
struct Node {
    token: Token,
    lhs: Option<Rc<Node>>,
    rhs: Option<Rc<Node>>,
    // @TODO: wrap a single integer as IntegerNode
    val: i32, // valid only when token is a number
}

impl Node {
    fn new(lhs: Node, rhs: Node, token: Token, val: i32) -> Self {
        Node {
            token,
            lhs: Some(lhs.into()),
            rhs: Some(rhs.into()),
            val,
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

    // for now, we consider infix expresseions and single number
    fn parse_expr(&mut self, precedence: Precedence) -> Node {
        // @Incomplete: for now, we assume the first token is an integer
        let mut expr: Node = self.parse_integer();
        loop {
            if precedence > self.peek_token().precedence() {
                return expr;
            }
            self.next_token();
            match self.cur_token().kind {
                TokenKind::TkPlus | TokenKind::TkMinus |
                TokenKind::TkMul | TokenKind::TkDiv => {
                    expr = self.parse_infix(expr)
                },
                TokenKind::TkNum => {
                    error_token(&self.src, self.cur_token(), "expect an operator");
                },
                TokenKind::TkEof => {
                    break;
                },
            }
        }
        expr
    }

    fn parse_integer(&self) -> Node {
        if self.cur_token().kind != TokenKind::TkNum {
            error_token(&self.src, self.cur_token(), "expect a number");
        }
        Node {
            token: self.cur_token().clone(),
            lhs: None,
            rhs: None,
            val: self.cur_token().val,
        }
    }

    fn parse_infix(&mut self, lhs: Node) -> Node {
        let tok = self.cur_token().clone();
        let p = tok.precedence();
        self.next_token();
        Node {
            token: tok,
            lhs: Some(lhs.into()),
            rhs: Some(self.parse_expr(p).into()),
            val: 0,
        }
    }
}

fn main() {
    // test case
    let src = String::from("10-1234*2-3423/12312-1223412+11112");
    println!("compiling source code: {src}");
    println!();

    let mut tokens: Vec<Token> = Vec::new();
    let tokens = tokenize(&src);
    let mut parser = Parser{
        src: src.clone(),
        tokens: tokens.clone(),
        cur_index: 0,
    };
    let res = parser.parse_expr(Precedence::Lowest);
    show_node(&Some(res.into()));
    println!();
    // error_token(&src, &res.token, "expect a number for the first token");

    // special case: deal with the first token
    if let TokenKind::TkNum = parser.cur_token().kind {
        println!("  .globl main");
        println!("main:");
        println!("  mov ${} %rax", parser.cur_token().val);
        parser.next_token();
    } else {
        error_token(&src, parser.cur_token(), "expect a number for the first token");
    }
    loop {
        match parser.cur_token().kind {
            TokenKind::TkPlus => {
                if let TokenKind::TkNum = parser.peek_token().kind  {
                    println!("  add ${}, %rax", parser.peek_token().val);
                    parser.next_token();
                    parser.next_token();
                } else {
                    error_token(&src, parser.peek_token(), "expect a number");
                }
            },
            TokenKind::TkMinus => {
                if let TokenKind::TkNum = parser.peek_token().kind  {
                    println!("  sub ${}, %rax", parser.peek_token().val);
                    parser.next_token();
                    parser.next_token();
                } else {
                    error_token(&src, parser.peek_token(), "expect a number");
                }
            },
            TokenKind::TkNum => {
                error_token(&src, parser.cur_token(), "expect an operator");
            },
            _ => {
                break;
            },
        }
    }
    println!("  ret");
}

fn show_node(root: &Option<Rc<Node>>) {
     if let Some(ref rc_root) = root {
        if rc_root.token.kind == TokenKind:: TkNum {
            // mid
            print!("{}", rc_root.token.content);
            return;
        }
        print!("(");
        // left
        if rc_root.lhs != None {
            show_node(&rc_root.lhs);
        }
        // mid
        print!("{}", rc_root.token.content);
        // right
        if rc_root.rhs != None {
            show_node(&rc_root.rhs);
        }
        print!(")");
    } else {
        print!("No value found");
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