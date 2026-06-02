use std::{io::{self, Write}, process::exit};
use colored::*;
use crate::lex::{self, *};
use crate::lex::Precedence::{self, *};
use crate::lex::TokenKind::{self, *};
use crate::lex::KeywordToken::{self, *};
use crate::lex::TypeSpecifier::{self, *};
use ExprType::*;
use crate::Type::{self, *};
use crate::SRC;
use crate::common::{self, *};

#[derive(Debug, Clone)]
pub enum StmtType {
    Ex(Expr),
    Return(Expr),
    Block(Vec<BlockItem>),
    If(IfStmt),
    For(ForStmt),
}
use StmtType::*;

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Box<StmtType>,
    pub otherwise: Option<Box<StmtType>>,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Option<Expr>,
    pub cond: Option<Expr>,
    pub inc: Option<Expr>,
    pub then: Box<StmtType>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Stmt(StmtType),
    Decl(Declaration),
}
use BlockItem::*;

#[derive(Debug, Clone)]
pub enum DeclaratorSuffix {
    ArrayLen(Vec<i32>),
    FunParam(Vec<Parameter>),
}
use DeclaratorSuffix::*;


#[derive(Debug, Clone)]
pub enum TranslationUnit {
    FunctionDef(Function),
    GlobalDecl(Declaration),
}
use TranslationUnit::*;

#[derive(Debug)]
pub struct Program {
    pub translation_units: Vec<TranslationUnit>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub name_span: Span,
    pub return_type: DeclarationSpecifier,
    pub star_count: i32,
    pub params: Vec<Parameter>,
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub decl_spec: DeclarationSpecifier,
    pub declarator: Declarator,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub decl_spec: DeclarationSpecifier,
    pub init_declarators: Vec<InitDeclarator>,
}

#[derive(Debug, Clone)]
pub enum DeclarationSpecifier {
    SpecInt,
    SpecChar,
}
use DeclarationSpecifier::*;

#[derive(Debug, Clone)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub init_expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Declarator {
    pub star_count: i32,
    pub name: String,
    pub suffix: Option<DeclaratorSuffix>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprType {
    Number(i32),
    Binary(Box<Expr>, Box<Expr>, TokenKind),
    Assign(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Deref(Box<Expr>),
    AddrOf(Box<Expr>),
    Ident(String),
    ArrayIndexing(Box<Expr>, Vec<Expr>),
    CommaExpression(Box<Expr>, Box<Expr>),
    FunCall(Box<Expr>, Vec<Expr>),
    Sizeof(Box<Expr>),
    Str(Vec<u8>),
    // Parenthesized expression: a transparent wrapper that records the span of
    // the enclosing parens for diagnostics while leaving the inner expression's
    // own span untouched. Semantically identical to `inner` — the analyzer
    // should unwrap it.
    Paren(Box<Expr>),
    StmtExpr(Vec<BlockItem>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub content: ExprType,
    pub span: Span,
}

impl Expr {
    pub fn new(content: ExprType, span: Span) -> Self {
        Expr{content, span}
    }
}


pub struct Parser {
    tokens: Vec<Token>,
    cur_index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            cur_index: 0,
        }
    }

    fn cur_token(&self) -> &Token {
        &self.tokens[self.cur_index]
    }

    fn next_token(&mut self) {
        self.cur_index += 1;
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.cur_index+1]
    }

    fn find_LBrace_before_encounter_semicolon(&self) -> Result<bool, String> {
        let mut index = self.cur_index;
        while self.tokens[index].kind != Semicolon {
            if self.tokens[index].kind == Eof {
                let err_msg = format!("parsing error: expect '{{' or ';', but got Eof");
                return Err(error_token(self.cur_token(), err_msg.as_str()));
            }
            if self.tokens[index].kind == LBrace {return Ok(true)};
            index += 1;
        }
        return Ok(false);
    }

    fn skip_cur_token(&mut self, expect_cur: &TokenKind) -> Result<(), String> {
        if self.cur_token_is(expect_cur.clone()) {
            self.next_token();
            Ok(())
        } else {
            let err_msg = format!("parsing error: expect {:?} kind token, but got {:?} kind token\n", expect_cur, self.cur_token().kind);
            return Err(error_token(self.cur_token(), err_msg.as_str()));
        }
    }

    fn next_token_is(&self, expect: TokenKind) -> bool {
        let peek = self.peek_token();
        return  peek.kind == expect;
    }

    fn cur_token_is(&self, expect: TokenKind) -> bool {
        let cur = self.cur_token();
        return  cur.kind == expect;
    }

    // Expect next token and jump to it if match expectation
    fn expect_peek(&mut self, expect: &TokenKind) -> Result<&Token, String> {
        let peek = self.peek_token();
        if  &peek.kind == expect {
            self.next_token();
            return Ok(self.cur_token());
        } else {
            let err_msg = format!("parsing error: expect {:?} kind token, but got {:?} kind token\n", expect, peek.kind);
            return Err(error_token(peek, err_msg.as_str()));
        }
    }

    fn check_jumpto_peek(&mut self, target: &TokenKind) -> Result<(), String> {
        match self.expect_peek(target) {
            Err(err_msg) => return Err(err_msg),
            _ => Ok(()),
        }
    }

    fn check_skip_peek(&mut self, target: &TokenKind) -> Result<(), String> {
        if let Err(err) = self.check_jumpto_peek(target) {
            return Err(err)
        } else {
            self.next_token();
            return Ok(())
        }
    }

    fn check_skip_current(&mut self, target: &TokenKind) -> Result<(), String> {
        let actual = &self.cur_token().kind;
        if actual == target {
            self.next_token();
            return Ok(())
        } else {
            let err_msg = error_token(self.cur_token(), format!("want '{:?}', but got '{:?}'", target, actual).as_str());
            return Err(err_msg)
        }
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut translation_units: Vec<TranslationUnit> = Vec::new();
        loop {
            if self.cur_token().kind == Eof {break};
            let find_LBrace = self.find_LBrace_before_encounter_semicolon()?;
            // Only function definition have LBrace, so if we hit LBrace then just start parsing function def.
            let current_unit = if find_LBrace {
                FunctionDef(self.parse_fun_def()?)
            // Others are all declarations.
            } else {
                GlobalDecl(self.parse_decl()?)
            };
            translation_units.push(current_unit);
            self.next_token();
        }
        Ok(Program{translation_units})
    }

    fn parse_decl(&mut self) -> Result<Declaration, String> {
        let decl_spec = self.parse_decl_spec()?;
        self.next_token();

        // parse init declarators
        let mut init_declarators: Vec<InitDeclarator> = Vec::new();
        loop {
            let declarator = self.parse_declarator()?;
            self.next_token();
            let mut init_expr = None;
            if let Assignment = self.cur_token().kind {
                self.next_token();
                let expr = self.parse_expr(Lowest)?;
                self.next_token();
                init_expr = Some(expr);
            }
            let init_declarator = InitDeclarator {declarator, init_expr};
            init_declarators.push(init_declarator);
            match self.cur_token().kind {
                Comma => {
                    self.next_token();
                    continue;
                }
                Semicolon => break,
                _ => {
                    let err_msg = error_token(self.cur_token(), "invalid token");
                    return Err(err_msg);
                },
            }
        }
        let declaration = Declaration{decl_spec, init_declarators};
        Ok(declaration)
    }

    fn parse_decl_spec(&mut self) -> Result<DeclarationSpecifier, String> {
        let kind = &self.cur_token().kind;
        let decl_spec: DeclarationSpecifier;
        match kind {
            Keyword(TypeSpecifier(Int)) => Ok(SpecInt),
            Keyword(TypeSpecifier(Char)) => Ok(SpecChar),
            _ => {
                let err_msg = error_token(self.cur_token(), "unknown declaration specifer!");
                return Err(err_msg);
            }
        }
    }

    fn parse_declarator(&mut self) -> Result<Declarator, String> {
        let mut star_count = 0;
        let name: String;
        // here, Mul is the pointer mark "*"
        while let Mul = self.cur_token().kind {
            star_count += 1;
            self.next_token();
        }
        if let LexIdent(ident) = &self.cur_token().kind {
            let start_index = self.cur_token().span.start_index;
            let mut end_index = self.cur_token().span.end_index;
            name = ident.clone();

            // parse suffix of a declarator
            let mut suffix = None;
            match &self.peek_token().kind {
                // array sizes
                LSqureBracket => {
                    let mut lens: Vec<i32> = Vec::new();
                    while &self.peek_token().kind == &LSqureBracket {
                        self.check_skip_peek(&LSqureBracket);
                        let cur_array_len: i32 = self.parse_raw_integer()?;
                        lens.push(cur_array_len);
                        self.expect_peek(&RSqureBracket);
                    }
                    end_index = self.cur_token().span.end_index;
                    suffix = Some(ArrayLen(lens));
                }
                // function parameters
                LParen => {
                    let mut params: Vec<Parameter> = Vec::new();
                    self.check_skip_peek(&LParen);
                    while self.cur_token().kind != RParen {
                        if &self.cur_token().kind == &Comma {
                            self.next_token();
                        }
                        let decl_spec = self.parse_decl_spec()?;
                        self.next_token();
                        let declarator = self.parse_declarator()?;
                        self.next_token();
                        let param = Parameter{decl_spec, declarator};
                        params.push(param);
                    }
                    end_index = self.cur_token().span.end_index;
                    suffix = Some(FunParam(params));
                }
                _ => {},
            }
            let span = Span{start_index, end_index};

            Ok(Declarator{star_count, name, suffix, span})
        } else {
            let err_msg = error_token(self.cur_token(), "not an identifier");
            return Err(err_msg);
        }
    }

    fn parse_stmt(&mut self) -> Result<StmtType, String> {
        let kind = &self.cur_token().kind;
        match kind {
            Keyword(Ret) => Ok(Return(self.parse_ret_stmt()?)),
            Keyword(KeywordToken::If) => Ok(StmtType::If(self.parse_if_stmt()?)),
            Keyword(KeywordToken::For) => Ok(StmtType::For(self.parse_for_stmt()?)),
            Keyword(While) => Ok(StmtType::For(self.parse_while_stmt()?)),
            Semicolon => Ok(Block(Vec::new())),
            LBrace => Ok(Block(self.parse_block()?)),
            _ => Ok(Ex(self.parse_expr_stmt()?)),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, String> {
        self.check_skip_peek(&LParen)?;
        // parse condition
        let cond = self.parse_expr(Lowest)?;
        self.check_skip_peek(&RParen)?;
        // parse then
        let then = Box::new(self.parse_stmt()?);
        self.next_token();
        // parse otherwise
        let otherwise = match self.cur_token().kind {
            Keyword(Else) => {
                self.next_token();
                Some(Box::new(self.parse_stmt()?))
            }
            _ => None
        };
        Ok(IfStmt{cond, then, otherwise})
    }

    fn parse_block(&mut self) -> Result<Vec<BlockItem>, String> {
        let mut items: Vec<BlockItem> = Vec::new();
        self.next_token(); // skip '{'
        let mut cur_kind: TokenKind = self.cur_token().kind.clone();
        while cur_kind != RBrace {
            let item: BlockItem;
            // @Future: if the token kind is a declaration-specifier (i.e., storage-class-specifier,
            // type-specifier or function-specifier), we parse decl.
            if matches!(cur_kind, Keyword(TypeSpecifier(_))) {
                item = Decl(self.parse_decl()?);
            } else {
                item = Stmt(self.parse_stmt()?);
            }
            items.push(item);
            // skip semicolon ';'
            self.next_token();
            if matches!(cur_kind, Eof) {
                return Err("parsing block statement error: reach the end of file.".to_string())
            }
            cur_kind = self.cur_token().kind.clone();
        }
        Ok(items)
    }

    fn parse_ret_stmt(&mut self) -> Result<Expr, String> {
        self.next_token();
        let expr = self.parse_expr(Lowest)?;
        self.expect_peek(&Semicolon)?;
        Ok(expr)
    }

    fn parse_for_stmt(&mut self) -> Result<ForStmt, String> {
        self.check_skip_peek(&LParen)?;
        let init: Option<Expr>;
        let cond: Option<Expr>;
        let inc: Option<Expr>;
        init  = match self.cur_token().kind {
            Semicolon => {
                self.check_skip_current(&Semicolon)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.check_skip_peek(&Semicolon)?;
                Some(expr)
            },
        };
        cond = match self.cur_token().kind {
            Semicolon => {
                self.check_skip_current(&Semicolon)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.check_skip_peek(&Semicolon)?;
                Some(expr)
            },
        };
        inc = match self.cur_token().kind {
            RParen => {
                self.check_skip_current(&RParen)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.check_skip_peek(&RParen)?;
                Some(expr)
            },
        };

        let then = self.parse_stmt()?;
        let then = Box::new(then);
        Ok(ForStmt{init, cond, inc, then})
    }

    fn parse_while_stmt(&mut self) -> Result<ForStmt, String> {
        self.check_skip_peek(&LParen)?;
        let cond: Option<Expr>;
        cond = match self.cur_token().kind {
            RParen => {
                self.check_skip_current(&RParen)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.check_skip_peek(&RParen)?;
                Some(expr)
            },
        };
        let then = self.parse_stmt()?;
        let then = Box::new(then);
        Ok(ForStmt{init: None, cond, inc: None, then})
    }

    fn parse_expr_stmt(&mut self) -> Result<Expr, String> {
        let expr = self.parse_expr(Lowest)?;
        self.expect_peek(&Semicolon)?;
        Ok(expr)
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
                        Comma => {
                            self.next_token();
                            expr = self.parse_comma_expression(expr)?;
                        },
                        LParen => {
                            self.next_token();
                            expr = self.parse_funcall(expr)?;
                        },
                        LSqureBracket => {
                            expr = self.parse_array_indexing(expr)?;
                        }
                        Assignment => {
                            self.next_token();
                            expr = self.parse_assign(expr)?;
                        }
                        Eof | RParen => {
                            break;
                        },
                        Num(_) => {
                            let err_msg = error_token(self.cur_token(), "expect an operator");
                            return Err(err_msg);
                        },
                        _ => {
                            let err_msg = error_token(self.peek_token(), "not support parsing this token");
                            return Err(err_msg);
                        },
                    }
                }
                Ok(expr)
            }
        }
    }

    fn parse_paren(&mut self) -> Result<Expr, String> {
        let start_index = self.cur_token().span.start_index;
        self.check_skip_current(&LParen)?;
        let inner = self.parse_expr(Precedence::Lowest)?;
        self.check_jumpto_peek(&RParen)?;
        let end_index = self.cur_token().span.end_index;
        // Wrap, don't mutate: the inner expression keeps its own span; the Paren
        // node carries the wider span that includes the parentheses.
        let span = Span{start_index, end_index};
        Ok(Expr::new(Paren(Box::new(inner)), span))
    }

    fn parse_prefix(&mut self) -> Result<Expr, String> {
        let cur_token_snapshot = self.cur_token().clone();
        match cur_token_snapshot.kind {
            LParen => {
                if self.peek_token().kind == LBrace {
                    return self.parse_stmt_expr();
                } else {
                    return self.parse_paren();
                }
            },
            Num(_) => self.parse_integer(),
            Plus => {
                self.next_token();
                self.parse_prefix()
            },
            Minus => {
                self.next_token();
                let operand = self.parse_prefix()?;
                let operand = Box::new(operand);
                let start_index = cur_token_snapshot.span.start_index;
                let end_index = self.cur_token().span.end_index;
                let span = Span{start_index, end_index};
                let expr = Expr::new(Neg(operand), span);
                Ok(expr)
            },
            Mul => {
                self.next_token();
                let operand = self.parse_prefix()?;
                let operand = Box::new(operand);
                let start_index = cur_token_snapshot.span.start_index;
                let end_index = self.cur_token().span.end_index;
                let span = Span{start_index, end_index};
                let expr = Expr::new(Deref(operand), span);
                Ok(expr)
            },
            Ampersand => {
                self.next_token();
                let operand = self.parse_prefix()?;
                let operand = Box::new(operand);
                let start_index = cur_token_snapshot.span.start_index;
                let end_index = self.cur_token().span.end_index;
                let span = Span{start_index, end_index};
                let expr = Expr::new(AddrOf(operand), span);
                Ok(expr)
            },
            Keyword(KeywordToken::Sizeof) => {
                self.next_token(); // skip "sizeof"
                let operand = self.parse_prefix()?;
                let operand = Box::new(operand);
                let start_index = cur_token_snapshot.span.start_index;
                let end_index = self.cur_token().span.end_index;
                let span = Span{start_index, end_index};
                let expr = Expr::new(ExprType::Sizeof(operand), span);
                Ok(expr)
            },
            LexIdent(_) => self.parse_ident(),
            StringLiteral(s) => self.parse_string(),
            _ => Err(error_token(&cur_token_snapshot, "parsing error: can't parse prefix expression here"))
        }
    }

    fn parse_stmt_expr(&mut self) -> Result<Expr, String> {
        let start_index = self.cur_token().span.start_index;
        self.skip_cur_token(&LParen);
        let items = self.parse_block()?;
        self.expect_peek(&RParen);
        let end_index = self.cur_token().span.end_index;
        let expr_span = Span{start_index, end_index};
        let content = StmtExpr(items);
        let expr = Expr::new(content, expr_span);
        Ok(expr)
    }
    
    fn parse_string(&mut self) -> Result<Expr, String> {
        let tok = self.cur_token().clone();
        if let StringLiteral(s) = &tok.kind {
            let mut content = Str(s.clone());
            let expr = Expr::new(content, tok.span);
            Ok(expr)
        } else {
            Err(error_token(self.cur_token(), "expect a string literal"))
        }
    }

    fn parse_ident(&mut self) -> Result<Expr, String> {
        let tok = self.cur_token().clone();
        if let LexIdent(name) = &tok.kind {
            let mut var = Ident(name.clone());
            let start_index = tok.span.start_index;
            let end_index = tok.span.end_index;
            let span = Span{start_index, end_index};
            let expr = Expr::new(var, span);
            Ok(expr)
        } else {
            Err(error_token(self.cur_token(), "expect an identifier"))
        }
    }

    fn parse_integer(&self) -> Result<Expr, String> {
        let tok = self.cur_token().clone();
        if let Num(n) = tok.kind {
            let span = Span {
                start_index: tok.span.start_index,
                end_index: tok.span.end_index,
                            };
            let expr = Expr::new(Number(n), span);
            return Ok(expr);
        } else {
            return Err(error_token(self.cur_token(), "expect a number"));
        }
    }

    fn parse_raw_integer(&self) -> Result<i32, String> {
        let cur_token = self.cur_token();
        if let Num(n) = cur_token.kind {
            return Ok(n);
        } else {
            return Err(error_token(self.cur_token(), "expect a number"));
        }
    }

    fn parse_infix(&mut self, lhs: Expr) -> Result<Expr, String> {
        let tok = self.cur_token().clone();
        let p = tok.precedence();
        self.next_token();
        let rhs = self.parse_expr(p)?;
        let span = Span {
            start_index: lhs.span.start_index,
            end_index: rhs.span.end_index,
                    };
        let content = Binary(Box::new(lhs), Box::new(rhs), tok.kind.clone());
        Ok(Expr::new(content, span))
    }

    fn parse_assign(&mut self, lhs: Expr) -> Result<Expr, String> {
        let tok = self.cur_token().clone();
        match lhs.content {
            Ident(_) | Deref(_) | ArrayIndexing(_, _) | Paren(_) => {
                self.next_token();
                let val = self.parse_expr(Lowest)?;
                let span = Span {
                    start_index: lhs.span.start_index,
                    end_index: val.span.end_index,
                                    };
                let content = ExprType::Assign(Box::new(lhs), Box::new(val));
                Ok(Expr::new(content, span))
            },
            _ => Err(error_span(lhs.span, "definitely not a lvalue name")),
        }
    }

    fn parse_comma_expression(&mut self, lhs:Expr) -> Result<Expr, String> {
        let start_index = lhs.span.start_index;
        self.next_token(); // skip ','
        let rhs = self.parse_expr(Lowest)?;
        let end_index = rhs.span.end_index;
        let span = Span {start_index, end_index};
        let content = ExprType::CommaExpression(Box::new(lhs), Box::new(rhs));
        return Ok(Expr::new(content, span));
    }

    fn parse_funcall(&mut self, lhs:Expr) -> Result<Expr, String> {
        let start_index = lhs.span.start_index;
        let args_list = self.parse_args()?;
        let end_index = self.cur_token().span.end_index;
        let span = Span {start_index, end_index};
        let content = FunCall(Box::new(lhs), args_list);
        return Ok(Expr::new(content, span));
    }

    fn parse_array_indexing(&mut self, lhs: Expr) -> Result<Expr, String> {
        let prime_token = self.cur_token().clone();
        let mut indices: Vec<Expr> = Vec::new();
        while self.peek_token().kind == LSqureBracket {
            self.check_skip_peek(&LSqureBracket);
            let cur_index = self.parse_expr(Lowest)?;
            indices.push(cur_index);
            self.expect_peek(&RSqureBracket);
        }
        let end_index = match indices.last() {
            None => prime_token.span.end_index,
            Some(expr) => expr.span.end_index,
        };
        let span = Span {
            start_index: lhs.span.start_index,
            end_index,
                    };
        let content = ArrayIndexing(Box::new(lhs), indices);
        return Ok(Expr::new(content, span))
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, String> {
        let mut args: Vec<Expr> = Vec::new();
        // skip LParen
        self.next_token();
        while(self.cur_token().kind != RParen) {
            let expr = self.parse_expr(Lowest)?;
            args.push(expr);
            self.next_token();
            let cur_tok = self.cur_token();
            match cur_tok.kind {
                Comma => self.next_token(),
                RParen => break,
                // error case:
                _ => {
                    let err_msg = format!("parsing function call error: expect comma as arguments separator or RParen
                    as enclosing of args list, but got {:?} kind token\n", cur_tok.kind);
                    return Err(error_token(cur_tok, err_msg.as_str()));
                }
            }
        }
        return Ok(args);
    }

    fn parse_fun_def(&mut self) -> Result<Function, String> {
        let return_type = self.parse_decl_spec()?;
        self.next_token();
        let declarator = self.parse_declarator()?;
        if let Some(FunParam(params)) = &declarator.suffix {
            self.expect_peek(&LBrace);
            let items = self.parse_block()?;
            Ok(Function{name: declarator.name, name_span: declarator.span, return_type,
                star_count: declarator.star_count, params: params.clone(), items})
        } else {
            Err(error_token(self.cur_token(), "error: declarator suffix is not function parameters"))
        }
    }

}

fn error_span(span: Span, info: &str) -> String {
    let mut err_msg = String::new();
    let (start_line, start_column, end_line, end_column) = span.locate();
    let extended_error_info = format!(":{}:{}: {}\n", start_line, start_column, info.red());
    err_msg.push_str(&extended_error_info);
    let start_line_content = get_src_content_at_line(start_line);
    err_msg.push_str(&start_line_content);
    err_msg.push_str("\n");
    let spaces = " ".repeat(start_column - 1);
    let arrows = if start_line == end_line {
        "^".repeat(span.end_index - span.start_index + 1)
    } else {
        "^".to_string()
    };
    err_msg.push_str(&format!("{}{}", spaces, arrows.red()));
    err_msg
}

fn error_token(tok: &Token, info: &str) -> String {
    error_span(tok.span, info)
}
