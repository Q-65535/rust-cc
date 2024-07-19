use std::{io::{self, Write}, collections::VecDeque, process::exit, mem::swap};
use crate::ExprType::{self, *};
use crate::StmtType::{self, *};
use crate::TokenKind::{self, *};
use crate::CompareToken::{self, *};
use crate::Expr;
use crate::Lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    TY_PTR(Box<Type>),
    TY_INT,
    ty_none,
}
use Type::*;

struct Obj {
    name: String,
    ty: Type,
}

struct SblTable {
    objs: Vec<Obj>,
}

impl SblTable {
    fn new() -> Self {
        SblTable {objs: Vec::new()}
    }

    fn find_obj(&self, s: &str) -> Option<&Obj> {
        for o in &self.objs {
            if o.name == s {
                return Some(o);
            }
        }
        None
    }

    fn add_new_obj(&mut self, name: &str, ty: Type) {
        let o = Obj{name: name.to_string(), ty};
        self.objs.push(o);
    }
}

pub struct Analyzer {
    sbl_table: SblTable,
}

impl Analyzer {

    pub fn new() -> Self {
        let sbl_table = SblTable::new();
        let mut analyzer = Analyzer{sbl_table};
        analyzer
    }

    pub fn analyze(&mut self, stmts: &mut Vec<StmtType>) {
        for stmt in stmts {
            self.analyze_stmt(stmt);
        }
    }

    fn analyze_stmt(&mut self, stmt: &mut StmtType) {
        match stmt {
            Ex(expr) | Return(expr) => self.analyze_expr(expr),
            Block(stmts) => self.analyze(stmts),
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
            Number(n) => expr.ty = TY_INT,
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
                            expr.ty = TY_INT;
                            scal_expr(expr, Div, 8);
                        }
                    }
                    _ => (),
                }
            }
            Assign(lhs, rhs) => {
                self.analyze_expr(rhs);
                // special case: add type for new variable (this case will be removed
                // after we implement declaration)
                // @Difference: for now in chibicc, every variable has type int
                if let Var(name)  = &lhs.content {
                    self.sbl_table.add_new_obj(name, rhs.ty.clone());
                }
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
                    TY_PTR(base) => expr.ty = *base.clone(),
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
    TY_PTR(base)
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
