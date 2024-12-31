use std::{io::{self, Write}, collections::VecDeque, process::exit, mem::swap};
use colored::*;
use crate::parse::{self, *};
use crate::lex::{self, *};
use crate::ExprType::*;
use crate::StmtType::*;
use crate::TokenKind::*;
use crate::CompareToken::*;
use crate::BlockItem::*;
use crate::DeclarationSpecifier::*;
use crate::Declaration;
use crate::Program;
use crate::Function;
use crate::Parameter;
use crate::Declarator;
use crate::DeclaratorSuffix;
use crate::Expr;
use crate::Lexer;
use crate::SRC;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // pointer to ... type
    TyPtr(Box<Type>),
    TyInt,
    ArrayOf(Box<Type>, i32),
    // function return ... type
    TyFunc(Box<Type>),
    ty_none,
}
use Type::*;

fn sizeof(ty: &Type) -> i32 {
    match ty {
        TyPtr(_) => 8,
        TyInt => 8,
        ArrayOf(element_ty, len) => sizeof(element_ty) * len,
        TyFunc(_) => 8,
        ty_none => 8,
    }
}

#[derive(Debug, Clone)]
pub struct Obj {
    name: String,
    ty: Type,
    // this offset should be based on %rbp
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
    // @Fix: these should be created for each function
    sbl_table: SblTable,
    cur_offset: i32,
}

impl Analyzer {
    pub fn new() -> Self {
        let sbl_table = SblTable::new();
        let mut analyzer = Analyzer{sbl_table, cur_offset: 0};
        analyzer
    }

    pub fn analyze(&mut self, mut program: Program) -> AnalyzedProgram {
        let mut afuns: Vec<AnalyzedFun> = Vec::new();
        for fun in program.funs {
            let mut fun_analyzer = FunAnalyzer::new();
            match fun_analyzer.analyze(fun) {
                Ok(afun) => afuns.push(afun),
                Err(e) => println!("{}", e),
            }
        }
        AnalyzedProgram{afuns}
    }
}

pub struct FunAnalyzer {
    sbl_table: SblTable,
    cur_offset: i32,
}

impl FunAnalyzer {
    pub fn new() -> Self {
        let sbl_table = SblTable::new();
        FunAnalyzer{sbl_table, cur_offset: 0}
    }

    pub fn analyze(&mut self, mut fun: Function) -> Result<AnalyzedFun, String> {
        for param in &fun.params {
            self.analyze_param(param)?;
        }
        for item in &mut fun.items {
            match item {
                Stmt(stmt) => self.analyze_stmt(stmt)?,
                Decl(decl) => self.analyze_decl(decl)?,
            }
        }
        Ok(AnalyzedFun{fun, sbl_table: self.sbl_table.clone(), stack_size: self.cur_offset})
    }

    fn analyze_items(&mut self, items: &mut Vec<BlockItem>) -> Result<(), String> {
        for item in items {
            match item {
                Stmt(stmt) => self.analyze_stmt(stmt)?,
                Decl(decl) => self.analyze_decl(decl)?,
            }
        }
        Ok(())
    }

    fn analyze_param(&mut self, param: &Parameter) -> Result<(), String> {
        let base_type: Type;
        match &param.decl_spec {
            SpecInt => base_type = TyInt,
        }
        let obj = self.create_obj(&base_type, &param.declarator.name);
        if let Some(_) = self.sbl_table.find_obj(&obj.name) {
            let err_info = format!("fatal error: parameter variable {} already defined", obj.name);
            return Err(self.err_declarator(&param.declarator, &err_info));
        }
        self.sbl_table.add_obj(obj);
        Ok(())
    }

    fn analyze_decl(&mut self, decl: &mut Declaration) -> Result<(), String> {
        let base_type: Type;
        match &decl.decl_spec {
            SpecInt => base_type = TyInt,
        }
        for init in &mut decl.init_declarators {
            if let Some(_) = self.sbl_table.find_obj(&init.declarator.name) {
                let err_info = format!("variable {} already defined", init.declarator.name);
                return Err(self.err_declarator(&init.declarator, &err_info));
            }
            // deal with pointers
            let mut cur_type = base_type.clone();
            for i in 0..init.declarator.star_count {
                cur_type = pointer_to(&cur_type);
            }
            // deal with suffix
            if let Some(suffix) = &mut init.declarator.suffix {
                match suffix {
                    DeclaratorSuffix::ArrayLen(lens) => {
                        while lens.len() > 0 {
                            let len: i32 = lens.pop().unwrap();
                            cur_type = array_of(&cur_type, len);
                        }
                    }
                    DeclaratorSuffix::FunParam(_) => todo!(),
                }
            }

            let obj = self.create_obj(&cur_type, &init.declarator.name);
            self.sbl_table.add_obj(obj);
            if let Some(expr) = &mut init.init_expr {
                // @Incomplete: check whether two types (obj and expr) match
                self.analyze_expr(expr)?;
            }
        }
        Ok(())
    }

    fn create_obj(&mut self, base_type: &Type, name: &str) -> Obj {
        let mut cur_type = base_type.clone();
        let mut size: i32 = sizeof(base_type);
        let obj = Obj{name: name.to_string(), ty: cur_type, offset: self.cur_offset};
        self.cur_offset += size;
        obj
    }

    fn analyze_stmt(&mut self, stmt: &mut StmtType) -> Result<(), String> {
        match stmt {
            Ex(expr) | Return(expr) => self.analyze_expr(expr)?,
            Block(items) => self.analyze_items(items)?,
            If{cond, then, otherwise} => {
                self.analyze_expr(cond)?;
                self.analyze_stmt(then);
                if let Some(otherwise) = otherwise {
                    self.analyze_stmt(otherwise);
                }
            }
            For{init, cond, inc, then} => {
                if let Some(init) = init {
                    self.analyze_expr(init)?;
                }
                if let Some(cond) = cond {
                    self.analyze_expr(cond)?;
                }
                if let Some(inc) = inc {
                    self.analyze_expr(inc)?;
                }
                self.analyze_stmt(then);
            }
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &mut Expr) -> Result<(), String> {
        // if already analyzed, skip
        if !(expr.ty == Type::ty_none) {
            return Ok(());
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
                        }
                        if lhs.is_ptr() && rhs.is_integer() {
                            let mut scal: i32 = 8;
                            match &lhs.ty {
                                TyPtr(..) => scal = sizeof(&lhs.ty),
                                ArrayOf(element_type, _) => scal = sizeof(element_type),
                                _ => {},
                            }
                            scal_expr(rhs, Mul, scal);
                        }
                        expr.ty = lhs.ty.clone();
                        return Ok(());
                    }
                    Minus => {
                        if lhs.is_integer() && rhs.is_ptr() {
                            return Err(self.error_expr(rhs, "error: integer - ptr"));
                        }
                        if lhs.is_ptr() && rhs.is_integer() {
                            let mut scal: i32 = 8;
                            match &lhs.ty {
                                TyPtr(..) => scal = sizeof(&lhs.ty),
                                ArrayOf(element_type, _) => scal = sizeof(element_type),
                                _ => {},
                            }
                            scal_expr(rhs, Mul, scal);
                            expr.ty = lhs.ty.clone();
                        } else if lhs.is_ptr() && rhs.is_ptr() {
                            // @Incomplete: consider array and pointer
                            expr.ty = TyInt;
                            let scal = sizeof(&lhs.ty);
                            scal_expr(expr, Div, scal);
                        } else if lhs.is_integer() && rhs.is_integer() {
                            expr.ty = TyInt;
                        }
                        Ok(())
                    }
                    _ => Ok(()),
                }
            }
            Assign(lhs, rhs) => {
                // @Incomplete: check whether two types (lhs and rhs) match
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
                    ArrayOf(base, _) => {
                        expr.ty = *base.clone();
                        Ok(())
                    }
                    _ => {
                        let err_msg = format!("semantic error: invalid dereferencing: 
                        try to dereference {:?}", expr.ty);
                        return Err(self.error_expr(expr, &err_msg));
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
            // array indexing is converted to pointer arithmatic
            ArrayIndexing(arr_ref, indices) => {
                self.analyze_expr(arr_ref)?;
                             // reborrow here
                for index in &mut *indices {
                    self.analyze_expr(index)?;
                }
                let mut cur_ref = arr_ref;
                for index in indices {
                    // type checking
                    if !cur_ref.is_ptr() {
                        let err_msg = self.error_expr(index, "subscripted value is neither array nor pointer nor vector");
                        return Err(err_msg);
                    } else {
                        let pointer_arithmatic = Binary(cur_ref.clone(), Box::new(index.clone()), Plus);
                        let mut pointer_arithmatic_expr = Expr::new(pointer_arithmatic, index.token.clone());
                        self.analyze_expr(&mut pointer_arithmatic_expr);
                        let deref = Deref(Box::new(pointer_arithmatic_expr));
                        let mut deref_expr = Expr::new(deref, index.token.clone());
                        self.analyze_expr(&mut deref_expr);
                        *cur_ref = Box::new(deref_expr);
                    }
                }
                expr.content = cur_ref.content.clone();
                Ok(())
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
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(expr.start);
        let arrows = "^".repeat(expr.end - expr.start);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }

    fn err_declarator(&self, declarator: &Declarator, info: &str) -> String {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
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

fn array_of(ty: &Type, len: i32) -> Type {
    let base = Box::new(ty.clone());
    ArrayOf(base, len)
}

fn depth_of(ty: &Type) -> i32 {
    let mut cur_ty = ty;
    let mut res = 0;
    while let ArrayOf(inner, _) = cur_ty {
        res += 1;
        cur_ty = inner;
    }
    res
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
