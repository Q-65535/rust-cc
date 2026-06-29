use std::{io::{self, Write}, process::exit};
use colored::*;
use crate::lex::{self, *};
use crate::lex::Precedence::{self, *};
use crate::lex::TokenKind::{self, *};
use ExprType::*;
use crate::SRC;
use crate::common::{self, *};

#[derive(Debug, Clone, PartialEq)]
pub enum StmtType {
    Ex(Expr),
    Return(Expr),
    Block(Vec<BlockItem>),
    If(IfStmt),
    For(ForStmt),
}
use StmtType::*;

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Box<StmtType>,
    pub otherwise: Option<Box<StmtType>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub init: Option<Expr>,
    pub cond: Option<Expr>,
    pub inc: Option<Expr>,
    pub then: Box<StmtType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Stmt(StmtType),
    Decl(Declaration),
}
use BlockItem::*;

#[derive(Debug, Clone, PartialEq)]
pub enum DeclaratorSuffix {
    ArrayLen(Vec<i32>),
    FunParam(Vec<Parameter>),
}
use DeclaratorSuffix::*;


#[derive(Debug, Clone, PartialEq)]
pub enum TranslationUnit {
    FunctionDef(Function),
    GlobalDecl(Declaration),
}
use TranslationUnit::*;

#[derive(Debug)]
pub struct Program {
    pub translation_units: Vec<TranslationUnit>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub name_span: Span,
    pub return_type: TypeSpec,
    pub star_count: i32,
    pub params: Vec<Parameter>,
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub decl_spec: TypeSpec,
    pub declarator: Declarator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub decl_spec: TypeSpec,
    pub declarators: Vec<Declarator>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub decl_spec: TypeSpec,
    pub declarator: Declarator,
}

#[derive(Debug, Clone, PartialEq)]
// @Rename: Actually, this should be renamed to TypeSpecifier.
// DeclarationSpecifier includes TypeSpecifier.
// @Refactor: We need a struct to contain this enum and store span info just like Expr.
pub enum TypeSpec {
    Int,
    Char,
    SpecStruct(StructSpecifier),
}
use TypeSpec::*;

#[derive(Debug, Clone, PartialEq)]
pub struct StructSpecifier {
    pub name: Option<String>,
    pub members: Option<Vec<Member>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declarator {
    pub star_count: i32,
    pub name: String,
    pub suffix: Option<DeclaratorSuffix>,
    pub init_expr: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Number(i32),
    Binary(Box<Expr>, Box<Expr>, TokenKind),
    Assign(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Deref(Box<Expr>),
    AddrOf(Box<Expr>),
    Ident(String),
    // ArrayIndexing(ref: Box<Expr>, index: Box<Expr>),
    ArrayIndexing(Box<Expr>, Vec<Expr>),
    RequestStructMember(Box<Expr>, String),
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

#[derive(Debug, Clone, PartialEq)]
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
    syntax_errors: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            cur_index: 0,
            syntax_errors: Vec::new(),
        }
    }

    fn cur_token(&self) -> &Token {
        &self.tokens[self.cur_index]
    }

    fn next_token(&mut self) {
        self.cur_index += 1;
    }

    fn advance(&mut self, n: usize) {
        self.cur_index += n;
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.cur_index+1]
    }

    fn find_LBrace_before_encounter_semicolon(&self) -> Result<bool, String> {
        let mut index = self.cur_index;
        while index != self.tokens.len() {
            if self.tokens[index].kind == LBrace {return Ok(true)};
            if self.tokens[index].kind == Semicolon {return Ok(false)};
            index += 1;
        }
        let error_message = format!("From here to eof, can't find ';' or '{{' to start parse!");
        return Err(error_token(self.cur_token(), &error_message));
    }

    fn consume(&mut self, expect_kind: &TokenKind) -> Result<(), String> {
        if &self.cur_token().kind == expect_kind {
            self.next_token();
            Ok(())
        } else {
            let err_msg = format!("expect {:?} kind token, but got {:?} kind token\n", expect_kind, self.cur_token().kind);
            return Err(error_token(self.cur_token(), err_msg.as_str()));
        }
    }

    // Expect next token and jump to it if match expectation
    fn jump_to_next_token(&mut self, expect: &TokenKind) -> Result<(), String> {
        let peek = self.peek_token();
        if  &peek.kind == expect {
            self.next_token();
            return Ok(());
        } else {
            let err_msg = format!("expect {:?} kind token, but got {:?} kind token\n", expect, peek.kind);
            return Err(error_token(peek, err_msg.as_str()));
        }
    }

    fn jump_over_next_token(&mut self, target: &TokenKind) -> Result<(), String> {
        if let Err(err) = self.jump_to_next_token(target) {
            return Err(err)
        } else {
            self.next_token();
            return Ok(())
        }
    }

    pub fn sync_parse_point(&mut self) {
        while self.cur_token().kind != Eof {
            if (matches!(self.cur_token().kind, Semicolon | RBrace | RParen | RSqureBracket)) {
                self.next_token();
                return;
            }
            if (matches!(self.cur_token().kind, LBrace | LParen | RSqureBracket)) {
                return;
            }
            self.next_token();
        }
    }

    pub fn parse(mut self) -> (Program, Vec<String>) {
        let mut translation_units: Vec<TranslationUnit> = Vec::new();
        while self.cur_token().kind != Eof {
            let result = self.find_LBrace_before_encounter_semicolon();
            let find_LBrace: bool;
            match result {
                Err(e) => {
                    self.syntax_errors.push(e);
                    return (Program{translation_units}, self.syntax_errors);
                }
                Ok(find) => find_LBrace = find,
            }
            // Only function definition have LBrace, so if we hit LBrace then just start parsing function def.
            if find_LBrace {
                match self.parse_fun_def() {
                    Err(error_message) => {
                        self.syntax_errors.push(error_message);
                        self.sync_parse_point();
                        continue;
                    },
                    Ok(fun) => translation_units.push(FunctionDef(fun)),
                }
            // Others are all declarations.
            } else {
                match self.parse_decl() {
                    Err(error_message) => {
                        self.syntax_errors.push(error_message);
                        self.sync_parse_point();
                        continue;
                    },
                    Ok(decl) => translation_units.push(GlobalDecl(decl)),
                }
            }
            self.next_token();
        }
        return (Program{translation_units}, self.syntax_errors);
    }

    fn parse_decl(&mut self) -> Result<Declaration, String> {
        let mut declarators: Vec<Declarator> = Vec::new();
        let decl_spec = self.parse_decl_spec()?;
        self.next_token();
        while self.cur_token().kind != Semicolon {
            let declarator = self.parse_declarator()?;
            declarators.push(declarator);
            self.next_token();
            match self.cur_token().kind {
                Comma => {
                    self.next_token();
                    continue;
                }
                Semicolon => break,
                _ => {
                    let err_msg = error_token(self.cur_token(), "parsing decl error: invalid token");
                    return Err(err_msg);
                },
            }
        }
        return Ok(Declaration{decl_spec, declarators});
    }

    fn parse_decl_spec(&mut self) -> Result<TypeSpec, String> {
        let kind = &self.cur_token().kind;
        let decl_spec: TypeSpec;
        match kind {
            TokenKind::Int => Ok(TypeSpec::Int),
            TokenKind::Char => Ok(TypeSpec::Char),
            TokenKind::Struct => {
                let struct_spec = self.parse_struct_specifier()?;
                Ok(SpecStruct(struct_spec))
            },
            _ => {
                let err_msg = error_token(self.cur_token(), "unknown declaration specifer!");
                return Err(err_msg);
            }
        }
    }

    fn parse_struct_specifier(&mut self) -> Result<StructSpecifier, String> {
        self.consume(&TokenKind::Struct);
        let mut struct_specifier = StructSpecifier{name: None, members: None};
        match &self.cur_token().clone().kind {
            LexIdent(name) => {
                struct_specifier.name = Some(name.clone());
                if self.peek_token().kind == LBrace {
                    self.next_token();
                    struct_specifier.members = Some(self.parse_struct_decl_list()?);
                }
            },
            LBrace => {
                struct_specifier.members = Some(self.parse_struct_decl_list()?);
            },
            _ => {
                let err_msg = error_token(self.cur_token(), "parsing struct specifier error: unknown token after 'struct'");
                return Err(err_msg);
            }
        }
        return Ok(struct_specifier);
    }

    fn parse_struct_decl_list(&mut self) -> Result<Vec<Member>, String> {
        self.consume(&LBrace);
        let mut members = Vec::new();
        while self.cur_token().kind != RBrace {
            let decl_spec = self.parse_decl_spec()?;
            self.next_token();
            // parse declarators separated by ','
            loop {
                let declarator = self.parse_declarator()?;
                let m = Member{decl_spec: decl_spec.clone(), declarator};
                members.push(m);
                if self.peek_token().kind == Comma {
                    self.jump_over_next_token(&Comma)?;
                } else {
                    break;
                }
            }
            self.jump_over_next_token(&Semicolon)?;
        }
        Ok(members)
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
                        self.jump_over_next_token(&LSqureBracket);
                        if let Num(n) = self.cur_token().kind {
                            let cur_array_len: i32 = self.parse_raw_integer()?;
                            lens.push(cur_array_len);
                        } else {
                            let err_msg = format!("expect a integer after square bracket in array decl");
                            return Err(error_token(self.cur_token(), &err_msg));
                        }
                        // parsing array declaration error
                        // let syntax_error_type = "while parsing array declaration: "
                        self.jump_to_next_token(&RSqureBracket);
                    }
                    end_index = self.cur_token().span.end_index;
                    suffix = Some(ArrayLen(lens));
                }
                // function parameters
                LParen => {
                    let mut params: Vec<Parameter> = Vec::new();
                    self.jump_over_next_token(&LParen);
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
            // This span doesn't include init_expr, just the declarator itself!
            // The init expr has its own span.
            let span = Span{start_index, end_index};
            let mut init_expr = None;
            if let Assignment = self.peek_token().kind {
                self.jump_to_next_token(&Assignment);
                let assignment_precedence = self.cur_token().precedence();
                self.consume(&Assignment);
                let expr = self.parse_expr(assignment_precedence)?;
                init_expr = Some(expr);
            }
            Ok(Declarator{star_count, name, suffix, init_expr, span})
        } else {
            let err_msg = error_token(self.cur_token(), "not an identifier");
            return Err(err_msg);
        }
    }

    fn parse_stmt(&mut self) -> Result<StmtType, String> {
        let kind = &self.cur_token().kind;
        match kind {
            TokenKind::Ret => Ok(Return(self.parse_ret_stmt()?)),
            TokenKind::If => Ok(StmtType::If(self.parse_if_stmt()?)),
            TokenKind::For => Ok(StmtType::For(self.parse_for_stmt()?)),
            TokenKind::While => Ok(StmtType::For(self.parse_while_stmt()?)),
            Semicolon => Ok(Block(Vec::new())),
            LBrace => Ok(Block(self.parse_block())),
            _ => Ok(Ex(self.parse_expr_stmt()?)),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, String> {
        self.jump_over_next_token(&LParen)?;
        // parse condition
        let cond = self.parse_expr(Lowest)?;
        self.jump_over_next_token(&RParen)?;
        // parse then
        let then = Box::new(self.parse_stmt()?);

        let mut otherwise = None;
        // parse otherwise (if it exists)
        if self.peek_token().kind == TokenKind::Else {
            self.advance(2);
            otherwise = Some(Box::new(self.parse_stmt()?));
        }
        Ok(IfStmt{cond, then, otherwise})
    }

    fn parse_block(&mut self) -> Vec<BlockItem> {
        let mut items: Vec<BlockItem> = Vec::new();
        self.next_token(); // skip '{'
        while self.cur_token().kind != RBrace {
            let item: BlockItem;
            // @Future: if the token kind is a declaration-specifier (i.e., storage-class-specifier,
            // type-specifier or function-specifier), we parse decl.
            if self.cur_token().is_decl_spec() {
                match self.parse_decl() {
                    Err(error_message) => {
                        self.syntax_errors.push(error_message);
                        self.sync_parse_point();
                        continue;
                    },
                    Ok(decl) => items.push(Decl(decl)),
                }
            } else {
                match self.parse_stmt() {
                    Err(error_message) => {
                        self.syntax_errors.push(error_message);
                        self.sync_parse_point();
                        continue;
                    },
                    Ok(stmt) => items.push(Stmt(stmt)),
                }
            }
            self.next_token(); // skip ';' or '}', ready to parse next BlockItem.
        }
        items
    }

    fn parse_ret_stmt(&mut self) -> Result<Expr, String> {
        debug_assert!(matches!(self.cur_token().kind, TokenKind::Ret));
        self.next_token();
        let expr = self.parse_expr(Lowest)?;
        self.jump_to_next_token(&Semicolon)?;
        Ok(expr)
    }

    fn parse_for_stmt(&mut self) -> Result<ForStmt, String> {
        self.jump_over_next_token(&LParen)?;
        let init: Option<Expr>;
        let cond: Option<Expr>;
        let inc: Option<Expr>;
        init  = match self.cur_token().kind {
            Semicolon => {
                self.consume(&Semicolon)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.jump_over_next_token(&Semicolon)?;
                Some(expr)
            },
        };
        cond = match self.cur_token().kind {
            Semicolon => {
                self.consume(&Semicolon)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.jump_over_next_token(&Semicolon)?;
                Some(expr)
            },
        };
        inc = match self.cur_token().kind {
            RParen => {
                self.consume(&RParen)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.jump_over_next_token(&RParen)?;
                Some(expr)
            },
        };

        let then = self.parse_stmt()?;
        let then = Box::new(then);
        Ok(ForStmt{init, cond, inc, then})
    }

    fn parse_while_stmt(&mut self) -> Result<ForStmt, String> {
        self.jump_over_next_token(&LParen)?;
        let cond: Option<Expr>;
        cond = match self.cur_token().kind {
            RParen => {
                self.consume(&RParen)?;
                None
            },
            _ => {
                let expr = self.parse_expr(Lowest)?;
                self.jump_over_next_token(&RParen)?;
                Some(expr)
            },
        };
        let then = self.parse_stmt()?;
        let then = Box::new(then);
        Ok(ForStmt{init: None, cond, inc: None, then})
    }

    fn parse_expr_stmt(&mut self) -> Result<Expr, String> {
        let expr = self.parse_expr(Lowest)?;
        self.jump_to_next_token(&Semicolon)?;
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
                        Eq | Neq | LT | LE | GT | GE => {
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
                        Period | Arrow => {
                            self.next_token();
                            expr = self.parse_request_struct_member(expr)?;
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
        self.consume(&LParen)?;
        let inner = self.parse_expr(Precedence::Lowest)?;
        self.jump_to_next_token(&RParen)?;
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
            Num(n) => Ok(self.parse_integer(n)),
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
            TokenKind::Sizeof => {
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
            _ => Err(error_token(&cur_token_snapshot, "can't parse prefix expression here"))
        }
    }

    fn parse_stmt_expr(&mut self) -> Result<Expr, String> {
        let start_index = self.cur_token().span.start_index;
        self.consume(&LParen);
        let items = self.parse_block();
        self.jump_to_next_token(&RParen);
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

    fn parse_integer(&self, n: i32) -> Expr {
        debug_assert!(matches!(self.cur_token().kind, Num(_)));
        let span = Span {
            start_index: self.cur_token().span.start_index,
            end_index: self.cur_token().span.end_index,
        };
        let expr = Expr::new(Number(n), span);
        return expr;
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
            Ident(_) | Deref(_) | ArrayIndexing(_, _) |
            Paren(_) | RequestStructMember(_, _) => {
                self.next_token();
                // Here we use right associatve rule when parsing assignment.
                let val = self.parse_expr(tok.precedence() - 1)?;
                let span = Span {
                    start_index: lhs.span.start_index,
                    end_index: val.span.end_index,
                                    };
                let content = ExprType::Assign(Box::new(lhs), Box::new(val));
                Ok(Expr::new(content, span))
            },
            _ => Err(syntax_error(lhs.span, "definitely not a lvalue name")),
        }
    }

    fn parse_request_struct_member(&mut self, lhs: Expr) -> Result<Expr, String> {
        let start_index = lhs.span.start_index;
        self.next_token(); // skip '.' or '->'
        let content: ExprType;
        match &self.cur_token().kind {
            LexIdent(name) => {
                content = ExprType::RequestStructMember(Box::new(lhs), name.clone());
            },
            _ => {
                let err_msg = format!("parsing error: parse_request_struct_member: expect a struct field name, but got {:?} token\n", self.cur_token().kind);
                return Err(error_token(self.cur_token(), err_msg.as_str()));
            },
        }
        let span = Span{
            start_index,
            end_index: self.cur_token().span.end_index,
        };
        let expr = Expr::new(content, span);

        if matches!(self.peek_token().kind, Arrow | Period) {
            self.next_token();
            return self.parse_request_struct_member(expr);
        } else {
            return Ok(expr);
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
            self.jump_over_next_token(&LSqureBracket);
            let cur_index = self.parse_expr(Lowest)?;
            indices.push(cur_index);
            self.jump_to_next_token(&RSqureBracket);
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
            // The comma here is an argument separator, not a comma operator.
            // parse_expr returns when peek's precedence is <= the passed precedence,
            // so passing Comma precedence makes it stop right at the next separator.
            let expr = self.parse_expr(precedence(&Comma))?;
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
            self.jump_to_next_token(&LBrace);
            let items = self.parse_block();
            Ok(Function{name: declarator.name, name_span: declarator.span, return_type,
                star_count: declarator.star_count, params: params.clone(), items})
        } else {
            Err(error_token(self.cur_token(), "error: declarator suffix is not function parameters"))
        }
    }

}

fn syntax_error(span: Span, err_msg: &str) -> String {
    let error_stage_info = "syntax error: ".to_string();
    error_span(span, &(error_stage_info+err_msg))
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
    syntax_error(tok.span, info)
}
