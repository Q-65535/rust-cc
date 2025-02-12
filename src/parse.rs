use std::{io::{self, Write}, process::exit};
use colored::*;
use crate::lex::{self, *};
use Precedence::*;
use TokenKind::*;
use ExprType::*;
use KeywordToken::*;
use crate::Type::{self, *};
use crate::SRC;

#[derive(Debug, Clone)]
pub enum StmtType {
    Ex(Expr),
    Return(Expr),
    Block(Vec<BlockItem>),
    If {cond: Expr, then: Box<StmtType>, otherwise: Option<Box<StmtType>>},
    For {init: Option<Expr>, cond: Option<Expr>, inc: Option<Expr>, then: Box<StmtType>},
}
use StmtType::*;

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

#[derive(Debug)]
pub struct Program {
    pub funs: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
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
    SpecInt
}
use DeclarationSpecifier::*;

#[derive(Debug, Clone)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    // Declaration may include initialization
    pub init_expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Declarator {
    pub star_count: i32,
    pub name: String,
    pub suffix: Option<DeclaratorSuffix>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    start: usize,
    len: usize,
    line_number: usize,
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
    ArrayIndexing(Box<Expr>, Vec<Expr>),
    FunCall(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub content: ExprType,
    // @Smell: Do we really need a token for each expression?
    pub token: Token,
    pub ty: Type,
    pub start: usize,
    pub end: usize,
}

impl Expr {
    pub fn new(content: ExprType, token: Token) -> Self {
        // @Improve: properly set initial type
        let mut expr = Expr{content, token, ty: ty_none, start: 0, end: 0};
        (expr.start, expr.end) = (expr.cal_start_index(), expr.cal_end_index());
        expr
    }

    fn cal_start_index(&self) -> usize {
        match &self.content {
            Number(_) | Neg(_) | Ident(_) | Deref(_) | AddrOf(_) => self.token.start,
            Binary(lhs, _, _) => lhs.cal_start_index(),
            ExprType::Assign(lhs, _) => lhs.cal_start_index(),
            ArrayIndexing(array_ref, _) => array_ref.cal_start_index(),
            FunCall(ident, _) => ident.cal_start_index(),
        }
    }

    fn cal_end_index(&self) -> usize {
        match &self.content {
            Neg(e) | Deref(e) | AddrOf(e) => e.cal_end_index(),
            Number(_) | Ident(_) => self.token.end,
            Binary(_, rhs, _) => rhs.cal_end_index(),
            ExprType::Assign(_, rhs) => rhs.cal_end_index(),
            ArrayIndexing(array_ref, _) => array_ref.cal_end_index(),
            FunCall(ident, args_list) => {
                match args_list.last() {
                    Some(last_arg) => return last_arg.cal_end_index(),
                    None => return ident.cal_end_index() + 2,
                }
            }
        }
    }

    pub fn is_integer(&self) -> bool {
        if let TyInt = &self.ty {
            true
        } else {
            false
        }
    }
    pub fn is_ptr(&self) -> bool {
        match &self.ty {
            TyPtr(_) | ArrayOf(_, _) => true,
            _ => false
        }
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

    fn skip_cur_token(&mut self, expect_cur: &TokenKind) -> Result<(), String> {
        if self.cur_token_is(expect_cur.clone()) {
            self.next_token();
            Ok(())
        } else {
            let err_msg = format!("parsing error: expect {:?} kind token, but got {:?} kind token\n", expect_cur, self.cur_token().kind);
            return Err(self.error_token(self.cur_token(), err_msg.as_str()));
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

    fn expect_peek(&mut self, expect: &TokenKind) -> Result<&Token, String> {
        let peek = self.peek_token();
        if  &peek.kind == expect {
            self.next_token();
            return Ok(self.cur_token());
        } else {
            let err_msg = format!("parsing error: expect {:?} kind token, but got {:?} kind token\n", expect, peek.kind);
            return Err(self.error_token(peek, err_msg.as_str()));
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
            let err_msg = self.error_token(self.cur_token(), format!("want '{:?}', but got '{:?}'", target, actual).as_str());
            return Err(err_msg)
        }
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut funs: Vec<Function> = Vec::new();
        loop {
            let kind = &self.cur_token().kind;
            match kind {
                Eof => break,
                _ => {
                    let fun = self.parse_fun_def()?;
                    funs.push(fun);
                }
            }
            self.next_token();
        } 
        Ok(Program{funs})
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
                    let err_msg = self.error_token(self.cur_token(), "invalid token");
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
            Keyword(Int) => Ok(SpecInt),
            _ => {
                let err_msg = self.error_token(self.cur_token(), "unknown declaration specifer!");
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
            let start = self.cur_token().start;
            let end = self.cur_token().end;
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
                    suffix = Some(FunParam(params));
                }
                _ => {},
            }

            Ok(Declarator{star_count, name, suffix, start, end})
        } else {
            let err_msg = self.error_token(self.cur_token(), "not an identifier");
            return Err(err_msg);
        }
    }

    fn parse_stmt(&mut self) -> Result<StmtType, String> {
        let kind = &self.cur_token().kind;
        match kind {
            Keyword(Ret) => Ok(self.parse_ret_stmt()?),
            Keyword(If) => Ok(self.parse_if_stmt()?),
            Keyword(For) => Ok(self.parse_for_stmt()?),
            Keyword(While) => Ok(self.parse_while_stmt()?),
            Semicolon => {
                let empty: Vec<BlockItem> = Vec::new();
                Ok(Block(empty))
            },
            LBrace => Ok(self.parse_block()?),
            _ => Ok(self.parse_expr_stmt()?),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<StmtType, String> {
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
        Ok(StmtType::If{cond, then, otherwise})
    }

    fn parse_block(&mut self) -> Result<StmtType, String> {
        let mut items: Vec<BlockItem> = Vec::new();
        // skip '{'
        self.next_token();
        let mut cur_kind: TokenKind = self.cur_token().kind.clone();
        while cur_kind != RBrace {
            let item: BlockItem;
            if cur_kind == Keyword(Int) {
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
        Ok(Block(items))
    }

    fn parse_ret_stmt(&mut self) -> Result<StmtType, String> {
        self.next_token();
        let expr = self.parse_expr(Lowest)?;
        self.expect_peek(&Semicolon)?;
        Ok(Return(expr))
    }

    fn parse_for_stmt(&mut self) -> Result<StmtType, String> {
        self.check_skip_peek(&LParen)?;
        let init: Option<Expr>;
        let cond: Option<Expr>;
        let inc: Option<Expr>;
        // @Duplication
        // @Duplication
        // @Duplication
        // @Duplication
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
        Ok(StmtType::For{init, cond, inc, then})
    }

    fn parse_while_stmt(&mut self) -> Result<StmtType, String> {
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
        Ok(StmtType::For{init: None, cond, inc: None, then})
    }

    fn parse_expr_stmt(&mut self) -> Result<StmtType, String> {
        let expr = self.parse_expr(Lowest)?;
        self.expect_peek(&Semicolon)?;
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
                            let err_msg = self.error_token(self.cur_token(), "expect an operator");
                            return Err(err_msg);
                        },
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
        self.check_skip_current(&LParen)?;
        let res = self.parse_expr(Precedence::Lowest);
        self.check_jumpto_peek(&RParen)?;
        res
    }

    fn parse_prefix(&mut self) -> Result<Expr, String> {
        let cur_token = self.cur_token().clone();
        match cur_token.kind {
            LParen => self.parse_paren(),
            Num(_) => self.parse_integer(),
            Plus => {
                self.next_token();
                self.parse_prefix()
            },
            Minus => {
                self.next_token();
                let prefix = self.parse_prefix()?;
                let operand = Box::new(prefix);
                let expr = Expr::new(Neg(operand), cur_token);
                Ok(expr)
            },
            Mul => {
                self.next_token();
                let prefix = self.parse_prefix()?;
                let operand = Box::new(prefix);
                let expr = Expr::new(Deref(operand), cur_token);
                Ok(expr)
            },
            Ampersand => {
                self.next_token();
                let prefix = self.parse_prefix()?;
                let operand = Box::new(prefix);
                let expr = Expr::new(AddrOf(operand), cur_token);
                Ok(expr)
            },
            LexIdent(_) => self.parse_ident(),
            _ => Err(self.error_token(&cur_token, "parsing error: can't parse prefix expression here"))
        }
    }
    
    fn parse_ident(&mut self) -> Result<Expr, String> {
        let tok = self.cur_token();
        if let LexIdent(name) = &tok.kind {
            let mut var = Ident(name.clone());
            let expr = Expr::new(var, tok.clone());
            Ok(expr)
        } else {
            Err(self.error_token(self.cur_token(), "expect an identifier"))
        }
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

    fn parse_raw_integer(&self) -> Result<i32, String> {
        let cur_token = self.cur_token();
        if let Num(n) = cur_token.kind {
            return Ok(n);
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
        match lhs.content {
            Ident(_) | Deref(_) | ArrayIndexing(_, _) => {
                self.next_token();
                let val = self.parse_expr(Lowest)?;
                let content = ExprType::Assign(Box::new(lhs), Box::new(val));
                Ok(Expr::new(content, tok))
            },
            _ => Err(self.error_token(&lhs.token, "not a lvalue name")),
        }
    }

    fn parse_funcall(&mut self, lhs:Expr) -> Result<Expr, String> {
        let cur_tok = self.cur_token().clone();
        let args_list = self.parse_args()?;
        let content = FunCall(Box::new(lhs), args_list);
        return Ok(Expr::new(content, cur_tok));
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
        let content = ArrayIndexing(Box::new(lhs), indices);
        return Ok(Expr::new(content, prime_token))
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
                    return Err(self.error_token(cur_tok, err_msg.as_str()));
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
            // parse function body
            let res = self.parse_block()?;
            // @Cleanup: parse_block should definitely return a code block, we don't need to
            // check it
            match res {
                Block(items) => Ok(Function{name: declarator.name, return_type,
                    star_count: declarator.star_count, params: params.clone(), items}),
                _ => Err(self.error_token(self.cur_token(), "(this is the end of
                parsing result) error parsing function definition: we want to
                parse code block of function definition, but got this result")),
            }
        } else {
            Err(self.error_token(self.cur_token(), "error: declarator suffix is not function parameters"))
        }
    }

    fn error_token(&self, tok: &Token, info: &str) -> String {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(tok.start);
        let arrows = "^".repeat(tok.end - tok.start);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }
}