use std::{io::{self, Write}, collections::VecDeque, process::exit, mem::swap};
use crate::ExprType::{self, *};
use crate::StmtType::{self, *};
use crate::TokenKind::{self, *};
use crate::CompareToken::{self, *};
use crate::BlockItem::{self, *};
use crate::DeclarationSpecifier::{self, *};
use crate::Declaration;
use crate::Function;
use crate::Declarator;
use crate::Expr;
use crate::Lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    TyPtr(Box<Type>),
    TyInt,
    ty_none,
}
use Type::*;

#[derive(Debug, Clone)]
pub struct Obj {
    name: String,
    ty: Type,
    pub offset: i32,
}

#[derive(Debug, Clone)]
pub struct SblTable {
    pub objs: Vec<Obj>,
}

impl SblTable {
    fn new() -> Self {
        SblTable {objs: Vec::new()}
    }

    pub fn find_obj(&self, s: &str) -> Option<&Obj> {
        for o in &self.objs {
            if o.name == s {
                return Some(o);
            }
        }
        None
    }

    pub fn add_obj(&mut self, o: Obj) {
        self.objs.push(o);
    }
}

pub struct AnalyzedFunc {
    pub func: Function,
    pub sbl_table: SblTable,
    pub stack_size: i32,
}

pub struct Analyzer {
    sbl_table: SblTable,
    cur_offset: i32,
}

impl Analyzer {

    pub fn new() -> Self {
        let sbl_table = SblTable::new();
        let mut analyzer = Analyzer{sbl_table, cur_offset: 0};
        analyzer
    }

    pub fn analyze(&mut self, mut func: Function) -> AnalyzedFunc {
        for item in &mut func.items {
            match item {
                Stmt(stmt) => self.analyze_stmt(stmt),
                Decl(decl) => self.analyze_decl(decl),
            }
        }
        AnalyzedFunc{func, sbl_table: self.sbl_table.clone(), stack_size: self.cur_offset}
    }

    fn analyze_items(&mut self, items: &mut Vec<BlockItem>) {
        for item in items {
            match item {
                Stmt(stmt) => self.analyze_stmt(stmt),
                Decl(decl) => self.analyze_decl(decl),
            }
        }
    }

    fn analyze_decl(&mut self, decl: &mut Declaration) {
        let base_type: Type;
        match &decl.decl_spec {
            SpecInt => base_type = TyInt,
        }
        for init in &mut decl.init_declarators {
            let obj = self.create_obj(&base_type, &init.declarator);
            if let Some(_) = self.sbl_table.find_obj(&obj.name) {
                println!("variable {} already defined", obj.name);
                exit(0);
            }
            self.sbl_table.add_obj(obj);
            if let Some(expr) = &mut init.init_expr {
                self.analyze_expr(expr);
            }
        }
    }

    fn create_obj(&mut self, base_type: &Type, declarator: &Declarator) -> Obj {
        let mut cur_type = base_type.clone();
        for i in 0..declarator.star_count {
            cur_type = pointer_to(&cur_type);
        }
        let obj = Obj{name: declarator.name.clone(), ty: cur_type, offset: self.cur_offset};
        self.cur_offset += 8;
        obj
    }

    fn analyze_stmt(&mut self, stmt: &mut StmtType) {
        match stmt {
            Ex(expr) | Return(expr) => self.analyze_expr(expr),
            Block(items) => self.analyze_items(items),
            If{cond, then, otherwise} => {
                self.analyze_expr(cond);
                self.analyze_stmt(then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(otherwise);
                }
            }
            For{init, cond, inc, then} => {
                if let Some(init) = init {
                    self.analyze_expr(init);
                }
                if let Some(cond) = cond {
                    self.analyze_expr(cond);
                }
                if let Some(inc) = inc {
                    self.analyze_expr(inc);
                }
                self.analyze_stmt(then);
            }
        }
    }

    fn analyze_expr(&mut self, expr: &mut Expr) {
        if !matches!(&expr.ty, Type::ty_none) {
            return;
        }
        match &mut expr.content {
            Number(n) => expr.ty = TyInt,
            Binary(lhs, rhs, tokenKind) => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
                // @TODO: check whether types of lhs and rhs match (when no pointer involved)
                match tokenKind {
                    // deal with pointer arithmatic
                    Plus => {
                        if lhs.is_ptr() && rhs.is_ptr() {
                            println!("error: both lhs and rhs are of ptr type");
                            exit(0);
                        }
                        if lhs.is_integer() && rhs.is_ptr() {
                            swap(lhs, rhs);
                            scal_expr(rhs, Mul, 8);
                            expr.ty = lhs.ty.clone();
                        }
                        if lhs.is_ptr() && rhs.is_integer() {
                            scal_expr(rhs, Mul, 8);
                            expr.ty = lhs.ty.clone();
                        }
                    }
                    Minus => {
                        if lhs.is_integer() && rhs.is_ptr() {
                            println!("error: integer - ptr");
                            exit(0);
                        }
                        if lhs.is_ptr() && rhs.is_integer() {
                            scal_expr(rhs, Mul, 8);
                            expr.ty = lhs.ty.clone();
                        } else if lhs.is_ptr() && rhs.is_ptr() {
                            expr.ty = TyInt;
                            scal_expr(expr, Div, 8);
                        }
                    }
                    _ => (),
                }
            }
            Assign(lhs, rhs) => {
                self.analyze_expr(rhs);
                self.analyze_expr(lhs);
                expr.ty = lhs.ty.clone();
            }
            Neg(val) => {
                self.analyze_expr(val);
                expr.ty = val.ty.clone();
            }
            Deref(val) => {
                self.analyze_expr(val);
                match &val.ty {
                    TyPtr(base) => expr.ty = *base.clone(),
                    _ => println!("dereferencing a non-pointer {:?}", val.content),
                }
            }
            AddrOf(val) => {
                self.analyze_expr(val);
                expr.ty = pointer_to(&val.ty);
            }
            Var(s) => {
                if let Some(o) = self.sbl_table.find_obj(s) {
                    expr.ty = o.ty.clone();
                } else {
                    // @Improve: better way to handle error
                    println!("analyze error: symbol {} not found", s);
                    exit(0);
                }
            }
        }
    }
}

fn pointer_to(ty: &Type) -> Type {
    let base = Box::new(ty.clone());
    TyPtr(base)
}

fn scal_expr(expr: &mut Expr, operation: TokenKind, scal: i32) {
    // expr for scal num
    let tok = Lexer::gen_token(Num(scal), "num_from_analyze", 0, 0);
    let num_expr_type = Number(scal);
    let num_expr = Expr::new(num_expr_type, tok);

    // scalled expr
    let new_expr_type = Binary(Box::new(expr.clone()), Box::new(num_expr), operation.clone());
    let tok = Lexer::gen_token(operation, "op_from_analyze", 0, 0);
    let mut new_expr = Expr::new(new_expr_type, tok);
    // type doesn't change
    new_expr.ty = expr.ty.clone();
    *expr = new_expr;
}
