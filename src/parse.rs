use std::{io::{self, Write}, process::exit};
use crate::*;
use crate::Precedence::*;
use crate::TokenKind::*;

pub enum StmtType {
    Ex(Expr),
}
use StmtType::*;

#[derive(PartialEq)]
pub struct Expr {
    pub content: ExprType,
    pub token: Token,
}

impl Expr {
    fn new(content: ExprType, token: Token) -> Self {
        Expr{content, token}
    }
}

#[derive(PartialEq)]
pub enum ExprType {
    Number(i32),
    Binary(Box<Expr>, Box<Expr>, TokenKind),
    Neg(Box<Expr>),
}


pub struct Parser {
    src: String,
    tokens: Vec<Token>,
    cur_index: usize,
}

impl Parser {
    pub fn new(src: String, tokens: Vec<Token>, cur_index: usize) -> Self {
        Parser {
            src,
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
            let err_msg = format!("parsing error: expect {:?} kind token, but got{:?} kind token\n", expect, peek.kind);
            return Err(error_token(&self.src, peek, err_msg.as_str()));
        }
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