use std::{io::{self, Write}, process::exit};
use colored::*;
use crate::Precedence::{self, *};
use crate::TokenKind::{self, *};
use crate::ExprType::*;
use crate::Token;

pub enum StmtType {
    Ex(Expr),
}
use StmtType::*;

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub content: ExprType,
    pub token: Token,
}

impl Expr {
    fn new(content: ExprType, token: Token) -> Self {
        Expr{content, token}
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprType {
    Number(i32),
    Binary(Box<Expr>, Box<Expr>, TokenKind),
    Assign(String, Box<Expr>),
    Neg(Box<Expr>),
    Var(String),
}


pub struct Parser {
    src: String,
    tokens: Vec<Token>,
    cur_index: usize,
}

impl Parser {
    pub fn new(src: &str, tokens: Vec<Token>, cur_index: usize) -> Self {
        Parser {
            src: src.to_string(),
            tokens,
            cur_index,
        }
    }

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

    fn expect_peek(&mut self, expect: TokenKind) -> Result<&Token, String> {
        let peek = self.peek_token();
        if  peek.kind == expect {
            self.next_token();
            return Ok(self.cur_token());
        } else {
            let err_msg = format!("parsing error: expect {:?} kind token, but got {:?} kind token\n", expect, peek.kind);
            return Err(self.error_token(peek, err_msg.as_str()));
        }
    }

    fn skip_peek(&mut self, target: &str) {
        let actual = self.peek_token().content.as_str();
        if  actual == target {
            self.cur_index += 1;
        } else {
            let err_msg = self.error_token(self.peek_token(), format!("want '{}', but got '{}'", target, actual).as_str());
            println!("{}", err_msg);
        }
    }

    fn skip(&mut self, target: &str) {
        let actual = self.cur_token().content.as_str();
        if actual == target {
            self.cur_index += 1;
        } else {
            self.error_token(self.cur_token(), format!("want '{}', but got '{}'", target, actual).as_str());
        }
    }

    pub fn parse(&mut self) -> Result<Vec<StmtType>, String> {
        let mut stmts: Vec<StmtType> = Vec::new();
        loop {
            match self.cur_token().kind {
                Eof => break,
                _ => {
                    let stmt = self.parse_stmt()?;
                    stmts.push(stmt);
                }
            }
            self.next_token();
        } 
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<StmtType, String> {
        match self.cur_token() {
            // @TODO: consider other types of statement
            _ => Ok(self.parse_expr_stmt()?),
        }
    }

    fn parse_expr_stmt(&mut self) -> Result<StmtType, String> {
        let expr = self.parse_expr(Lowest)?;
        self.expect_peek(Semicolon)?;
        Ok(Ex(expr))
    }

    pub fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, String> {
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
                        Compare(_) => {
                            self.next_token();
                            expr = self.parse_infix(expr)?;
                        },
                        Num(_) | LParen => {
                            let err_msg = self.error_token(self.cur_token(), "expect an operator");
                            return Err(err_msg);
                        },
                        Eof | RParen => {
                            break;
                        },
                        Assignment => {
                            self.next_token();
                            expr = self.parse_assign(expr)?;
                        }
                        _ => {
                            let err_msg = self.error_token(self.peek_token(), "not support parsing this token");
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
            Ident => return self.parse_ident(),
            _ => {
                let err_msg = self.error_token(&cur_token, "can't parse prefix expression here");
                return Err(err_msg);
            }
        }
    }
    
    fn parse_ident(&self) -> Result<Expr, String> {
        let tok = self.cur_token();
        let expr = Expr::new(Var(tok.content.clone()), tok.clone());
        Ok(expr)
    }

    fn parse_integer(&self) -> Result<Expr, String> {
        let cur_token = self.cur_token();
        if let Num(n) = cur_token.kind {
            let expr = Expr::new(Number(n), cur_token.clone());
            return Ok(expr);
        } else {
            return Err(self.error_token(self.cur_token(), "expect a number"));
        }
    }

    fn parse_infix(&mut self, lhs: Expr) -> Result<Expr, String> {
        let tok = self.cur_token().clone();
        let p = tok.precedence();
        self.next_token();
        let rhs = self.parse_expr(p)?;
        let content = Binary(Box::new(lhs), Box::new(rhs), tok.kind.clone());
        Ok(Expr::new(content, tok))
    }

    fn parse_assign(&mut self, lhs: Expr) -> Result<Expr, String> {
        let tok = self.cur_token().clone();
        if let Var(s) = lhs.content {
            self.next_token();
            let val = self.parse_expr(Lowest)?;
            let content = ExprType::Assign(s, Box::new(val));
            return Ok(Expr::new(content, tok));
        } else {
            return Err(self.error_token(&lhs.token, "not a variable name"));
        }
    }

    fn error_token(&self, tok: &Token, info: &str) -> String {
        let mut err_msg = String::from("");
        err_msg.push_str(&format!("{}\n", self.src));
        let spaces = " ".repeat(tok.index);
        let arrows = "^".repeat(tok.len);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows, info.red()));
        err_msg
    }
}