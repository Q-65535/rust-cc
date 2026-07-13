use colored::*;
use crate::lex::*;
use crate::lex::Precedence::{self, *};
use crate::lex::TokenKind::{self, *};
use ExprType::*;
use crate::SRC;
use crate::common::*;

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
    ArrayLen(usize, Option<Box<DeclaratorSuffix>>),
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
    pub return_type_specifier: Vec<Decl_Spec>,
    pub declarator: Declarator,
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub decl_spec: Vec<Decl_Spec>,
    pub declarator: Declarator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub decl_spec: Vec<Decl_Spec>,
    pub declarators: Vec<Declarator>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub decl_spec: Vec<Decl_Spec>,
    pub declarator: Declarator,
}

#[derive(Debug, Clone, PartialEq)]
// @Refactor: We need a struct to contain this enum and store span info just like Expr.
pub enum Decl_Spec {
    Typedef,
    Typedef_Name(String),
    Int,
    Long,
    Short,
    Char,
    Void,
    Struct_Union(Struct_Union_Specifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Struct_Or_Union {
    Is_Struct,
    Is_Union,
}
use Struct_Or_Union::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Struct_Union_Specifier {
    pub kind: Struct_Or_Union,
    pub name: Option<String>,
    pub members: Option<Vec<Member>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Abstract_Declarator {
    pub star_count: i32,
    pub direct_abstract_declarator: Option<Box<Abstract_Declarator>>,
    pub suffix: Option<DeclaratorSuffix>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Abstract_Direct_Declarator {
    Paren_Enclosed_Abstract_Declarator(Abstract_Declarator),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declarator {
    pub star_count: i32,
    pub direct_declarator: Box<Direct_Declarator>,
    pub suffix: Option<DeclaratorSuffix>,
    pub init_expr: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Direct_Declarator {
    Identifier(String),
    Paren_Enclosed_Declarator(Declarator),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Natural_Number(u64),
    Binary(Box<Expr>, Box<Expr>, TokenKind),
    Assign(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Deref(Box<Expr>),
    AddrOf(Box<Expr>),
    Ident(String),
    ArrayIndexing(Box<Expr>, Box<Expr>),
    RequestStructMember(Box<Expr>, String),
    CommaExpression(Box<Expr>, Box<Expr>),
    FunCall(Box<Expr>, Vec<Expr>),
    Sizeof_Expr(Box<Expr>),
    Sizeof_Type_Name(Type_Name),
    Str(Vec<u8>),
    // Parenthesized expression: a transparent wrapper that records the span of
    // the enclosing parens for diagnostics while leaving the inner expression's
    // own span untouched. Semantically identical to `inner` — the analyzer
    // should unwrap it.
    Paren(Box<Expr>),
    StmtExpr(Vec<BlockItem>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type_Name {
    pub decl_specs: Vec<Decl_Spec>,
    // @TODO: rename to abstract_declarator
    pub declarator: Option<Abstract_Declarator>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub typedef_names: Vec<String>,
}

impl Scope {
    pub fn new() -> Self {
        Scope{
            typedef_names: Vec::new(),
        }
    }

    pub fn add_typedef_name(&mut self, name: &str) {
        self.typedef_names.push(name.to_string());
    }

    pub fn is_typedef_name(&self, name: &str) -> bool {
        for typedef_name in &self.typedef_names {
            if typedef_name == name {
                return true;
            }
        }
        return false;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeManager {
    pub scopes: Vec<Scope>,
    pub current_scope_index: usize,
}

impl ScopeManager {
    pub fn new() -> Self {
        let mut scopes: Vec<Scope> = Vec::new();
        scopes.push(Scope::new());
        ScopeManager{scopes, current_scope_index: 0}
    }

    pub fn enter_new_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.current_scope_index += 1;
    }

    pub fn exit_current_scope(&mut self) {
        self.scopes.pop();
        self.current_scope_index -= 1;
    }

    pub fn is_typedef_name(&self, name: &str) -> bool {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if current_scope.is_typedef_name(name) == true {
                return true;
            }
        }
        return false;
    }

    pub fn is_typedef_name_in_current_scope(&self, name: &str) -> bool {
        let current_scope = &self.scopes[self.current_scope_index];
        return current_scope.is_typedef_name(name);
    }

    pub fn add_typedef_name(&mut self, name: &str) {
        let current_scope = &mut self.scopes[self.current_scope_index];
        current_scope.add_typedef_name(name);
    }
}


pub struct Parser {
    tokens: Vec<Token>,
    cur_index: usize,
    scope_manager: ScopeManager,
    syntax_errors: Vec<String>,
    // This is just for the convenience for debugging.
    // It just shows where we are parsing now.
    cur_parsing_context: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let starting_context = token_to_context(&tokens[0]);
        Parser {
            tokens,
            cur_index: 0,
            scope_manager: ScopeManager::new(),
            syntax_errors: Vec::new(),
            cur_parsing_context: starting_context,
        }
    }

    // Cursor contract: cur_index points to the first unconsumed token. Every
    // parse_* function consumes the grammar production it owns and returns
    // with this cursor on the first token outside that production.
    fn cur_token(&self) -> &Token {
        &self.tokens[self.cur_index]
    }

    fn previous_token(&self) -> &Token {
        &self.tokens[self.cur_index - 1]
    }

    fn at(&self, kind: &TokenKind) -> bool {
        &self.cur_token().kind == kind
    }

    fn bump(&mut self) -> Token {
        let token = self.cur_token().clone();
        if token.kind != Eof {
            self.cur_index += 1;
            // Uncomment this when you want to see where we are
            // parsing now while using a debugger by examine self.cur_parsing_context.
            // self.cur_parsing_context = token_to_context(self.cur_token());
        }
        token
    }

    fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn peek_token(&self) -> &Token {
        self.tokens.get(self.cur_index + 1).unwrap_or(self.cur_token())
    }

    fn expect(&mut self, expect_kind: &TokenKind) -> Result<Token, String> {
        if self.at(expect_kind) {
            Ok(self.bump())
        } else {
            let err_msg = format!(
                "expect {:?} kind token, but got {:?} kind token\n",
                expect_kind,
                self.cur_token().kind,
            );
            Err(error_token(self.cur_token(), &err_msg))
        }
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

    // @Fix: While parsing a function definition, if the data type of params is unknown,
    // the program will trap into a infinite loop.
    pub fn sync_parse_point(&mut self) {
        while !matches!(self.cur_token().kind, Semicolon | RBrace | Eof) {
            self.bump();
        }

        if self.at(&Semicolon) {
            self.bump();
        }
    }

    pub fn parse(mut self) -> (Program, Vec<String>) {
        let mut translation_units: Vec<TranslationUnit> = Vec::new();
        while !self.at(&Eof) {
            let start_index = self.cur_index;
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
                        if self.at(&RBrace) || self.cur_index == start_index {
                            self.bump();
                        }
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
                        if self.at(&RBrace) || self.cur_index == start_index {
                            self.bump();
                        }
                        continue;
                    },
                    Ok(decl) => translation_units.push(GlobalDecl(decl)),
                }
            }
            debug_assert!(self.cur_index > start_index);
        }
        return (Program{translation_units}, self.syntax_errors);
    }

    fn parse_decl(&mut self) -> Result<Declaration, String> {
        let mut declarators: Vec<Declarator> = Vec::new();
        let decl_spec = self.parse_decl_specs()?;
        let is_typedef_declaration = decl_spec.iter().any(|spec| *spec == Decl_Spec::Typedef);

        if !self.at(&Semicolon) {
            loop {
                let declarator = self.parse_init_declarator()?;
                if is_typedef_declaration {
                    let name = get_declarator_name(&declarator);
                    if self.scope_manager.is_typedef_name_in_current_scope(name) {
                        let err_msg = error_span(
                            declarator.span,
                            "typedef name is already being used!",
                        );
                        return Err(err_msg);
                    }
                    self.scope_manager.add_typedef_name(name);
                }
                declarators.push(declarator);

                if !self.eat(&Comma) {
                    break;
                }
            }
        }

        self.expect(&Semicolon)?;
        Ok(Declaration{decl_spec, declarators})
    }

    fn is_decl_spec(&self, token: &Token) -> bool {
        match &token.kind {
            (Struct | Union | Int | Long | Short | Char | Void | Typedef) => true,
            LexIdent(name) => self.scope_manager.is_typedef_name(name),
            _ => false,
        }
    }

    fn is_type_spec(&self, token: &Token) -> bool {
        match &token.kind {
            (Struct | Union | Int | Long | Short | Char | Void) => true,
            LexIdent(name) => self.scope_manager.is_typedef_name(name),
            _ => false,
        }
    }

    fn parse_decl_specs(&mut self) -> Result<Vec<Decl_Spec>, String> {
        debug_assert!(self.is_decl_spec(self.cur_token()));
        let mut decl_specs = Vec::new();
        while self.is_decl_spec(self.cur_token()) {
            let cur_decl_spec = match self.cur_token().kind.clone() {
                TokenKind::Typedef => {
                    self.bump();
                    Decl_Spec::Typedef
                },
                TokenKind::LexIdent(name) => {
                    self.bump();
                    Decl_Spec::Typedef_Name(name)
                },
                TokenKind::Int => {
                    self.bump();
                    Decl_Spec::Int
                },
                TokenKind::Long => {
                    self.bump();
                    Decl_Spec::Long
                },
                TokenKind::Short => {
                    self.bump();
                    Decl_Spec::Short
                },
                TokenKind::Char => {
                    self.bump();
                    Decl_Spec::Char
                },
                TokenKind::Void => {
                    self.bump();
                    Decl_Spec::Void
                },
                TokenKind::Struct | TokenKind::Union => {
                    let struct_spec = self.parse_struct_union_specifier()?;
                    Decl_Spec::Struct_Union(struct_spec)
                },
                _ => {
                    let err_msg = error_token(self.cur_token(), "unknown declaration specifer!");
                    return Err(err_msg);
                }
            };
            decl_specs.push(cur_decl_spec);
        }
        Ok(decl_specs)
    }

    fn parse_struct_union_specifier(&mut self) -> Result<Struct_Union_Specifier, String> {
        let keyword = self.bump();
        let kind = match keyword.kind {
            Union => Is_Union,
            Struct => Is_Struct,
            _ => return Err(error_token(&keyword, "expected 'struct' or 'union'")),
        };

        let mut struct_specifier = Struct_Union_Specifier{kind, name: None, members: None};

        if let LexIdent(name) = self.cur_token().kind.clone() {
            self.bump();
            struct_specifier.name = Some(name);
        }

        if self.at(&LBrace) {
            struct_specifier.members = Some(self.parse_struct_decl_list()?);
        } else if struct_specifier.name.is_none() {
            return Err(error_token(
                self.cur_token(),
                "parsing struct specifier error: expected a tag or member list",
            ));
        }

        Ok(struct_specifier)
    }

    fn parse_struct_decl_list(&mut self) -> Result<Vec<Member>, String> {
        self.expect(&LBrace)?;
        let mut members = Vec::new();
        while !matches!(self.cur_token().kind, RBrace | Eof) {
            let decl_spec = self.parse_decl_specs()?;
            // parse declarators separated by ','
            loop {
                let declarator = self.parse_declarator()?;
                let m = Member{decl_spec: decl_spec.clone(), declarator};
                members.push(m);
                if !self.eat(&Comma) {
                    break;
                }
            }
            self.expect(&Semicolon)?;
        }

        self.expect(&RBrace)?;
        Ok(members)
    }

    fn parse_declarator(&mut self) -> Result<Declarator, String> {
        let start_index = self.cur_token().span.start_index;
        let mut star_count = 0;
        while self.eat(&Mul) {
            star_count += 1;
        }

        let direct_declarator = match self.cur_token().kind.clone() {
            LexIdent(ident) => {
                self.bump();
                Box::new(Direct_Declarator::Identifier(ident))
            },
            LParen => {
                self.expect(&LParen)?;
                let paren_enclosed_declarator = self.parse_declarator()?;
                self.expect(&RParen)?;
                Box::new(Direct_Declarator::Paren_Enclosed_Declarator(paren_enclosed_declarator))
            },
            _ => {
                return Err(error_token(
                    self.cur_token(),
                    "unable to parse declarator here: not an identifier or a LParen.",
                ));
            },
        };

        let suffix = if matches!(self.cur_token().kind, LSqureBracket | LParen) {
            Some(self.parse_declarator_suffix()?)
        } else {
            None
        };

        // This span doesn't include init_expr, just the declarator itself.
        let end_index = self.previous_token().span.end_index;
        let span = Span{start_index, end_index};

        Ok(Declarator{
            star_count,
            direct_declarator,
            suffix,
            init_expr: None,
            span,
        })
    }

    fn parse_init_declarator(&mut self) -> Result<Declarator, String> {
        let mut declarator = self.parse_declarator()?;
        if self.eat(&Assignment) {
            // Commas separate declarators, so leave a comma unconsumed here.
            declarator.init_expr = Some(self.parse_expr(precedence(&Comma))?);
        }
        Ok(declarator)
    }

    fn parse_declarator_suffix(&mut self) -> Result<DeclaratorSuffix, String> {
        debug_assert!(matches!(self.cur_token().kind, LSqureBracket | LParen));

        match self.cur_token().kind {
            // array sizes
            LSqureBracket => {
                self.expect(&LSqureBracket)?;
                if matches!(self.cur_token().kind, Lex_Natural_Num(_)) {
                    let cur_array_len: usize = self.parse_raw_usize()?;
                    self.expect(&RSqureBracket)?;
                    if matches!(self.cur_token().kind, LSqureBracket | LParen) {
                        let inner_suffix = self.parse_declarator_suffix()?;
                        Ok(ArrayLen(cur_array_len, Some(Box::new(inner_suffix))))
                    } else {
                        Ok(ArrayLen(cur_array_len, None))
                    }
                } else {
                    Err(error_token(
                        self.cur_token(),
                        "expect an unsigned integer after square bracket in array decl",
                    ))
                }
            },
            // function parameters
            LParen => {
                self.expect(&LParen)?;
                let mut params: Vec<Parameter> = Vec::new();

                while !matches!(self.cur_token().kind, RParen | Eof) {
                    let decl_spec = self.parse_decl_specs()?;
                    let declarator = self.parse_declarator()?;
                    let param = Parameter{decl_spec, declarator};
                    params.push(param);

                    if !self.eat(&Comma) {
                        break;
                    }
                }

                self.expect(&RParen)?;
                Ok(FunParam(params))
            },
            _ => {
                Err(error_token(self.cur_token(), "Can't parse declarator suffix here!"))
            },
        }
    }

    fn parse_stmt(&mut self) -> Result<StmtType, String> {
        match self.cur_token().kind {
            TokenKind::Ret => Ok(Return(self.parse_ret_stmt()?)),
            TokenKind::If => Ok(StmtType::If(self.parse_if_stmt()?)),
            TokenKind::For => Ok(StmtType::For(self.parse_for_stmt()?)),
            TokenKind::While => Ok(StmtType::For(self.parse_while_stmt()?)),
            Semicolon => {
                self.expect(&Semicolon)?;
                Ok(Block(Vec::new()))
            },
            LBrace => Ok(Block(self.parse_block())),
            _ => Ok(Ex(self.parse_expr_stmt()?)),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, String> {
        debug_assert!(self.at(&TokenKind::If));
        self.expect(&TokenKind::If)?;
        self.expect(&LParen)?;
        let cond = self.parse_expr(Lowest)?;
        self.expect(&RParen)?;
        let then = Box::new(self.parse_stmt()?);

        let otherwise = if self.eat(&Else) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(IfStmt{cond, then, otherwise})
    }

    fn parse_block(&mut self) -> Vec<BlockItem> {
        self.scope_manager.enter_new_scope();
        let mut items: Vec<BlockItem> = Vec::new();
        debug_assert!(self.at(&LBrace));
        self.bump();

        while !matches!(self.cur_token().kind, RBrace | Eof) {
            let start_index = self.cur_index;
            // @Future: if the token kind is a declaration-specifier (i.e., storage-class-specifier,
            // type-specifier or function-specifier), we parse decl.
            if self.is_decl_spec(self.cur_token()) {
                match self.parse_decl() {
                    Err(error_message) => {
                        self.syntax_errors.push(error_message);
                        self.sync_parse_point();
                        if self.cur_index == start_index
                            && !matches!(self.cur_token().kind, RBrace | Eof) {
                            self.bump();
                        }
                        continue;
                    },
                    Ok(decl) => items.push(Decl(decl)),
                }
            } else {
                match self.parse_stmt() {
                    Err(error_message) => {
                        self.syntax_errors.push(error_message);
                        self.sync_parse_point();
                        if self.cur_index == start_index
                            && !matches!(self.cur_token().kind, RBrace | Eof) {
                            self.bump();
                        }
                        continue;
                    },
                    Ok(stmt) => items.push(Stmt(stmt)),
                }
            }

            debug_assert!(self.cur_index > start_index);
        }

        if self.at(&RBrace) {
            self.bump();
        } else {
            self.syntax_errors.push(error_token(
                self.cur_token(),
                "expected '}' to close block",
            ));
        }

        self.scope_manager.exit_current_scope();
        items
    }

    fn parse_ret_stmt(&mut self) -> Result<Expr, String> {
        self.expect(&TokenKind::Ret)?;
        let expr = self.parse_expr(Lowest)?;
        self.expect(&Semicolon)?;
        Ok(expr)
    }

    fn parse_for_stmt(&mut self) -> Result<ForStmt, String> {
        self.expect(&TokenKind::For)?;
        self.expect(&LParen)?;

        let init = if self.eat(&Semicolon) {
            None
        } else {
            let expr = self.parse_expr(Lowest)?;
            self.expect(&Semicolon)?;
            Some(expr)
        };

        let cond = if self.eat(&Semicolon) {
            None
        } else {
            let expr = self.parse_expr(Lowest)?;
            self.expect(&Semicolon)?;
            Some(expr)
        };

        let inc = if self.eat(&RParen) {
            None
        } else {
            let expr = self.parse_expr(Lowest)?;
            self.expect(&RParen)?;
            Some(expr)
        };

        let then = Box::new(self.parse_stmt()?);
        Ok(ForStmt{init, cond, inc, then})
    }

    fn parse_while_stmt(&mut self) -> Result<ForStmt, String> {
        self.expect(&While)?;
        self.expect(&LParen)?;

        let cond = if self.eat(&RParen) {
            None
        } else {
            let expr = self.parse_expr(Lowest)?;
            self.expect(&RParen)?;
            Some(expr)
        };

        let then = Box::new(self.parse_stmt()?);
        Ok(ForStmt{init: None, cond, inc: None, then})
    }

    fn parse_expr_stmt(&mut self) -> Result<Expr, String> {
        let expr = self.parse_expr(Lowest)?;
        self.expect(&Semicolon)?;
        Ok(expr)
    }

    pub fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, String> {
        let mut expr = self.parse_prefix()?;

        while precedence < self.cur_token().precedence() {
            expr = match self.cur_token().kind {
                Plus | Minus | Mul | Div | Eq | Neq | LT | LE | GT | GE => {
                    self.parse_infix(expr)?
                },
                Comma => self.parse_comma_expression(expr)?,
                LParen => self.parse_funcall(expr)?,
                LSqureBracket => self.parse_array_indexing(expr)?,
                Assignment => self.parse_assign(expr)?,
                Period | Arrow => self.parse_request_struct_member(expr)?,
                _ => {
                    return Err(error_token(
                        self.cur_token(),
                        "not support parsing this token",
                    ));
                },
            };
        }

        Ok(expr)
    }

    fn parse_paren(&mut self) -> Result<Expr, String> {
        let open_paren = self.expect(&LParen)?;
        let inner = self.parse_expr(Precedence::Lowest)?;
        let close_paren = self.expect(&RParen)?;
        // Wrap, don't mutate: the inner expression keeps its own span; the Paren
        // node carries the wider span that includes the parentheses.
        let span = Span{
            start_index: open_paren.span.start_index,
            end_index: close_paren.span.end_index,
        };
        Ok(Expr::new(Paren(Box::new(inner)), span))
    }

    fn parse_prefix(&mut self) -> Result<Expr, String> {
        // @Rename: prefix_start_token
        let cur_token_snapshot = self.cur_token().clone();
        match cur_token_snapshot.kind {
            LParen => {
                if self.peek_token().kind == LBrace {
                    return self.parse_stmt_expr();
                } else {
                    return self.parse_paren();
                }
            },
            Lex_Natural_Num(n) => Ok(self.parse_natural_number(n)),
            Plus => {
                self.bump();
                self.parse_prefix()
            },
            Minus => {
                self.bump();
                let operand = self.parse_prefix()?;
                let span = Span{
                    start_index: cur_token_snapshot.span.start_index,
                    end_index: operand.span.end_index,
                };
                let expr = Expr::new(Neg(Box::new(operand)), span);
                Ok(expr)
            },
            Mul => {
                self.bump();
                let operand = self.parse_prefix()?;
                let span = Span{
                    start_index: cur_token_snapshot.span.start_index,
                    end_index: operand.span.end_index,
                };
                let expr = Expr::new(Deref(Box::new(operand)), span);
                Ok(expr)
            },
            Ampersand => {
                self.bump();
                let operand = self.parse_prefix()?;
                let span = Span{
                    start_index: cur_token_snapshot.span.start_index,
                    end_index: operand.span.end_index,
                };
                let expr = Expr::new(AddrOf(Box::new(operand)), span);
                Ok(expr)
            },
            TokenKind::Sizeof => {
                self.expect(&Sizeof)?;
                let (expr_content, end_index) = if self.at(&LParen)
                    && self.is_type_spec(self.peek_token()) {
                    self.expect(&LParen)?;
                    let type_name = self.parse_type_name()?;
                    let close_paren = self.expect(&RParen)?;
                    (Sizeof_Type_Name(type_name), close_paren.span.end_index)
                } else {
                    let operand = self.parse_prefix()?;
                    let end_index = operand.span.end_index;
                    (Sizeof_Expr(Box::new(operand)), end_index)
                };
                let span = Span{
                    start_index: cur_token_snapshot.span.start_index,
                    end_index,
                };
                let expr = Expr::new(expr_content, span);
                Ok(expr)
            },
            LexIdent(_) => self.parse_ident(),
            StringLiteral(s) => self.parse_string(),
            _ => Err(error_token(&cur_token_snapshot, "can't parse prefix expression here"))
        }
    }

    fn parse_type_name(&mut self) -> Result<Type_Name, String> {
        let decl_specs = self.parse_decl_specs()?;
        let declarator = if Self::starts_abstract_declarator(&self.cur_token().kind) {
            Some(self.parse_abstract_declarator()?)
        } else {
            None
        };

        Ok(Type_Name{decl_specs, declarator})
    }

    fn starts_abstract_declarator(kind: &TokenKind) -> bool {
        matches!(kind, Mul | LSqureBracket | LParen)
    }

    fn starts_grouped_abstract_declarator(&self) -> bool {
        self.at(&LParen) && Self::starts_abstract_declarator(&self.peek_token().kind)
    }

    // Enters on the first token of the abstract declarator and returns on the
    // first token that does not belong to it.
    fn parse_abstract_declarator(&mut self) -> Result<Abstract_Declarator, String> {
        let start_index = self.cur_token().span.start_index;
        let mut star_count = 0;

        while self.eat(&Mul) {
            star_count += 1;
        }

        let direct_abstract_declarator = if self.starts_grouped_abstract_declarator() {
            self.expect(&LParen)?;
            let inner_declarator = self.parse_abstract_declarator()?;
            self.expect(&RParen)?;
            Some(Box::new(inner_declarator))
        } else {
            None
        };

        let suffix = if matches!(self.cur_token().kind, LSqureBracket | LParen) {
            Some(self.parse_declarator_suffix()?)
        } else {
            None
        };

        let end_index = self.previous_token().span.end_index;
        let span = Span{start_index, end_index};

        Ok(Abstract_Declarator{star_count, direct_abstract_declarator, suffix, span})
    }

    fn parse_stmt_expr(&mut self) -> Result<Expr, String> {
        let open_paren = self.expect(&LParen)?;
        let items = self.parse_block();
        let close_paren = self.expect(&RParen)?;
        let expr_span = Span{
            start_index: open_paren.span.start_index,
            end_index: close_paren.span.end_index,
        };
        let content = StmtExpr(items);
        let expr = Expr::new(content, expr_span);
        Ok(expr)
    }
    
    fn parse_string(&mut self) -> Result<Expr, String> {
        let tok = self.bump();
        if let StringLiteral(s) = &tok.kind {
            let content = Str(s.clone());
            let expr = Expr::new(content, tok.span);
            Ok(expr)
        } else {
            Err(error_token(&tok, "expect a string literal"))
        }
    }

    fn parse_ident(&mut self) -> Result<Expr, String> {
        let tok = self.bump();
        if let LexIdent(name) = &tok.kind {
            let expr = Expr::new(Ident(name.clone()), tok.span);
            Ok(expr)
        } else {
            Err(error_token(&tok, "expect an identifier"))
        }
    }

    fn parse_natural_number(&mut self, n: u64) -> Expr {
        debug_assert!(matches!(self.cur_token().kind, Lex_Natural_Num(_)));
        let token = self.bump();
        Expr::new(Natural_Number(n), token.span)
    }

    fn parse_raw_usize(&mut self) -> Result<usize, String> {
        let token = self.bump();
        if let Lex_Natural_Num(n) = token.kind {
            Ok(n.try_into().unwrap())
        } else {
            Err(error_token(&token, "expect a number"))
        }
    }

    fn parse_infix(&mut self, lhs: Expr) -> Result<Expr, String> {
        let infix_operator = self.bump().kind;
        let p = precedence(&infix_operator);
        let rhs = self.parse_expr(p)?;
        let span = Span {
            start_index: lhs.span.start_index,
            end_index: rhs.span.end_index,
        };
        let content = Binary(Box::new(lhs), Box::new(rhs), infix_operator);
        Ok(Expr::new(content, span))
    }

    fn parse_assign(&mut self, lhs: Expr) -> Result<Expr, String> {
        if !matches!(
            &lhs.content,
            Ident(_) | Deref(_) | ArrayIndexing(_, _) | Paren(_) | RequestStructMember(_, _)
        ) {
            return Err(syntax_error(lhs.span, "definitely not a lvalue name"));
        }

        let assignment = self.expect(&Assignment)?;
        // Subtract one precedence level to make assignment right-associative.
        let val = self.parse_expr(assignment.precedence() - 1)?;
        let span = Span {
            start_index: lhs.span.start_index,
            end_index: val.span.end_index,
        };
        let content = ExprType::Assign(Box::new(lhs), Box::new(val));
        Ok(Expr::new(content, span))
    }

    fn parse_request_struct_member(&mut self, lhs: Expr) -> Result<Expr, String> {
        let start_index = lhs.span.start_index;
        self.bump(); // skip '.' or '->'
        let member_token = self.bump();
        match member_token.kind {
            LexIdent(name) => {
                let content = ExprType::RequestStructMember(Box::new(lhs), name);
                let span = Span{
                    start_index,
                    end_index: member_token.span.end_index,
                };
                Ok(Expr::new(content, span))
            },
            _ => {
                let err_msg = format!(
                    "parsing error: parse_request_struct_member: expect a struct field name, but got {:?} token\n",
                    member_token.kind,
                );
                Err(error_token(&member_token, &err_msg))
            },
        }
    }

    fn parse_comma_expression(&mut self, lhs:Expr) -> Result<Expr, String> {
        let start_index = lhs.span.start_index;
        self.expect(&Comma)?;
        let rhs = self.parse_expr(Lowest)?;
        let end_index = rhs.span.end_index;
        let span = Span {start_index, end_index};
        let content = ExprType::CommaExpression(Box::new(lhs), Box::new(rhs));
        Ok(Expr::new(content, span))
    }

    fn parse_funcall(&mut self, lhs:Expr) -> Result<Expr, String> {
        let start_index = lhs.span.start_index;
        let args_list = self.parse_args()?;
        let end_index = self.previous_token().span.end_index;
        let span = Span {start_index, end_index};
        let content = FunCall(Box::new(lhs), args_list);
        Ok(Expr::new(content, span))
    }

    fn parse_array_indexing(&mut self, lhs: Expr) -> Result<Expr, String> {
        self.expect(&LSqureBracket)?;
        let the_index = self.parse_expr(Lowest)?;
        let close_bracket = self.expect(&RSqureBracket)?;

        let span = Span {
            start_index: lhs.span.start_index,
            end_index: close_bracket.span.end_index,
        };
        let content = ArrayIndexing(Box::new(lhs), Box::new(the_index));
        Ok(Expr::new(content, span))
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, String> {
        let mut args: Vec<Expr> = Vec::new();
        self.expect(&LParen)?;

        if self.eat(&RParen) {
            return Ok(args);
        }

        loop {
            // The comma here is an argument separator, not a comma operator.
            // Passing comma precedence leaves the separator unconsumed.
            let expr = self.parse_expr(precedence(&Comma))?;
            args.push(expr);

            if self.eat(&Comma) {
                continue;
            }

            self.expect(&RParen)?;
            break;
        }

        Ok(args)
    }

    fn parse_fun_def(&mut self) -> Result<Function, String> {
        let return_type_specifier = self.parse_decl_specs()?;
        let declarator = self.parse_declarator()?;
        if let Some(FunParam(params)) = &declarator.suffix {
            if !self.at(&LBrace) {
                return Err(error_token(self.cur_token(), "expected function body"));
            }
            let items = self.parse_block();
            Ok(Function{return_type_specifier, declarator, items})
        } else {
            Err(error_token(self.cur_token(), "error: declarator suffix is not function parameters"))
        }
    }
}

fn get_declarator_name(declarator: &Declarator) -> &str {
    match &*declarator.direct_declarator {
        Direct_Declarator::Identifier(name) => {return name;}
        Direct_Declarator::Paren_Enclosed_Declarator(inner_declarator) => {
            return get_declarator_name(inner_declarator);
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

// This is just a utility function used to visually show where the given token is in
// the source code.
fn token_to_context(tok: &Token) -> String {
    let span = tok.span;
    let mut context_info = String::new();
    let (start_line, start_column, end_line, end_column) = span.locate();
    let location_info = format!(":{}:{}:\n", start_line, start_column);
    context_info.push_str(&location_info);
    let start_line_content = get_src_content_at_line(start_line);
    context_info.push_str(&start_line_content);
    context_info.push_str("\n");
    let spaces = " ".repeat(start_column - 1);
    let arrows = if start_line == end_line {
        "^".repeat(span.end_index - span.start_index + 1)
    } else {
        "^".to_string()
    };
    context_info.push_str(&format!("{}{}", spaces, arrows));
    context_info
}
