use std::{io::{self, Write}, collections::VecDeque, process::exit, mem::swap};
use colored::*;
use crate::ExprType::{self, *};
use crate::StmtType::{self, *};
use crate::TokenKind::{self, *};
use crate::CompareToken::{self, *};
use crate::BlockItem::{self, *};
use crate::DeclarationSpecifier::{self, *};
use crate::Declaration;
use crate::Program;
use crate::Function;
use crate::Declarator;
use crate::Expr;
use crate::Lexer;
use crate::SRC;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // pointer to ... type
    TyPtr(Box<Type>),
    TyInt,
    // function return ... type
    TyFunc(Box<Type>),
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

pub struct AnalyzedProgram {
    pub afuns: Vec<AnalyzedFun>,
}

#[derive(Clone)]
pub struct AnalyzedFun {
    pub fun: Function,
    pub sbl_table: SblTable,
    pub stack_size: i32,
}

pub struct Analyzer {
    src: String,
    // @Fix: these should be created for each function
    sbl_table: SblTable,
    cur_offset: i32,
}

impl Analyzer {
    pub fn new() -> Self {
        let sbl_table = SblTable::new();
        let gsrc = SRC.lock().unwrap();
        let mut analyzer = Analyzer{src: gsrc.clone(), sbl_table, cur_offset: 0};
        analyzer
    }

    pub fn analyze(&mut self, mut program: Program) -> AnalyzedProgram {
        let mut afuns: Vec<AnalyzedFun> = Vec::new();
        for fun in program.funs {
            let mut fun_analyzer = FunAnalyzer::new();
            let afun = fun_analyzer.analyze(fun);
            afuns.push(afun);
        }
        AnalyzedProgram{afuns}
    }
}

pub struct FunAnalyzer {
    src: String,
    sbl_table: SblTable,
    cur_offset: i32,
}

impl FunAnalyzer {
    pub fn new() -> Self {
        let sbl_table = SblTable::new();
        let gsrc = SRC.lock().unwrap();
        FunAnalyzer{src: gsrc.clone(), sbl_table, cur_offset: 0}
    }

    pub fn analyze(&mut self, mut fun: Function) -> AnalyzedFun {
        for item in &mut fun.items {
            match item {
                Stmt(stmt) => self.analyze_stmt(stmt),
                Decl(decl) => self.analyze_decl(decl),
            }
        }
        AnalyzedFun{fun, sbl_table: self.sbl_table.clone(), stack_size: self.cur_offset}
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
                let err_info = format!("variable {} already defined", obj.name);
                println!("{}", self.err_declarator(&init.declarator, &err_info));
                exit(0);
            }
            self.sbl_table.add_obj(obj);
            if let Some(expr) = &mut init.init_expr {
                if let Err(e) = self.analyze_expr(expr) {
                    println!("{}", e);
                    exit(0);
                }
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
            Ex(expr) | Return(expr) => {
                if let Err(e) = self.analyze_expr(expr) {
                    println!("{}", e);
                    exit(0);
                }
            }
            Block(items) => {
                self.analyze_items(items);
            }
            If{cond, then, otherwise} => {
                if let Err(e) = self.analyze_expr(cond) {
                    println!("{}", e);
                    exit(0);
                }
                self.analyze_stmt(then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(otherwise);
                }
            }
            For{init, cond, inc, then} => {
                if let Some(init) = init {
                    if let Err(e) = self.analyze_expr(init) {
                        println!("{}", e);
                        exit(0);
                    }
                }
                if let Some(cond) = cond {
                    if let Err(e) = self.analyze_expr(cond) {
                        println!("{}", e);
                        exit(0);
                    }
                }
                if let Some(inc) = inc {
                    if let Err(e) = self.analyze_expr(inc) {
                        println!("{}", e);
                        exit(0);
                    }
                }
                self.analyze_stmt(then);
            }
        }
    }

    fn analyze_expr(&mut self, expr: &mut Expr) -> Result<(), String> {
        if !matches!(&expr.ty, Type::ty_none) {
            return Err(self.error_expr(expr, "error: both lhs and rhs are of ptr type"));
        }
        match &mut expr.content {
            Number(n) => Ok(expr.ty = TyInt),
            Binary(lhs, rhs, tokenKind) => {
                self.analyze_expr(lhs)?;
                self.analyze_expr(rhs)?;
                // @TODO: check whether types of lhs and rhs match (when no pointer involved)
                match tokenKind {
                    // deal with pointer arithmatic
                    Plus => {
                        if lhs.is_ptr() && rhs.is_ptr() {
                            // @TODO: report the error as a single error message
                            let left_err = self.error_expr(lhs, "error: both lhs and rhs are of ptr type");
                            let right_err = self.error_expr(rhs, "error: both lhs and rhs are of ptr type");
                            return Err(left_err + &right_err);
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
                        return Ok(());
                    }
                    Minus => {
                        if lhs.is_integer() && rhs.is_ptr() {
                            return Err(self.error_expr(rhs, "error: integer - ptr"));
                        }
                        if lhs.is_ptr() && rhs.is_integer() {
                            scal_expr(rhs, Mul, 8);
                            expr.ty = lhs.ty.clone();
                        } else if lhs.is_ptr() && rhs.is_ptr() {
                            expr.ty = TyInt;
                            scal_expr(expr, Div, 8);
                        }
                        Ok(())
                    }
                    _ => Ok(()),
                }
            }
            Assign(lhs, rhs) => {
                self.analyze_expr(rhs)?;
                self.analyze_expr(lhs)?;
                expr.ty = lhs.ty.clone();
                Ok(())
            }
            Neg(val) => {
                self.analyze_expr(val)?;
                expr.ty = val.ty.clone();
                Ok(())
            }
            Deref(val) => {
                self.analyze_expr(val)?;
                match &val.ty {
                    TyPtr(base) => {
                        expr.ty = *base.clone();
                        Ok(())
                    }
                    _ => {
                        return Err(self.error_expr(expr, "semantic error: dereferencing a non-pointer"));
                    }
                }
            }
            AddrOf(val) => {
                self.analyze_expr(val)?;
                expr.ty = pointer_to(&val.ty);
                Ok(())
            }
            Ident(s) => {
                if let Some(o) = self.sbl_table.find_obj(s) {
                    expr.ty = o.ty.clone();
                    Ok(())
                } else {
                    let err_info = format!("semantic error: symbol '{}' not found", s);
                    return Err(self.error_expr(expr, &err_info));
                }
            }
            FunCall(ident, args) => {
                for arg in args {
                    self.analyze_expr(arg)?;
                }
                // the function name may be in another elf file, we don't check its validaity
                Ok(())
            }
        }
    }

    fn error_expr(&self, expr: &Expr, info: &str) -> String {
        let mut err_msg = String::from("");
        err_msg.push_str(&format!("{}\n", self.src));
        let spaces = " ".repeat(expr.start);
        let arrows = "^".repeat(expr.end - expr.start);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }

    fn err_declarator(&self, declarator: &Declarator, info: &str) -> String {
        let gsrc = SRC.lock().unwrap();
        let src = gsrc.to_string();
        let mut err_msg = String::from("");
        err_msg.push_str(&format!("{}\n", src));
        let spaces = " ".repeat(declarator.start);
        let arrows = "^".repeat(declarator.end - declarator.start);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }
}

fn pointer_to(ty: &Type) -> Type {
    let base = Box::new(ty.clone());
    TyPtr(base)
}

fn function_type(ty: &Type) -> Type {
    let return_type = Box::new(ty.clone());
    TyFunc(return_type)
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
