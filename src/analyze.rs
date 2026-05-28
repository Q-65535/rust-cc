use std::{io::{self, Write}, collections::VecDeque, process::exit, mem::swap};
use colored::*;
use crate::parse::{self, *};
use crate::lex::{self, *};
use crate::ir;
use ExprType::*;
use StmtType::*;
use TokenKind::*;
use CompareToken::*;
use BlockItem::*;
use DeclarationSpecifier::*;
use crate::SRC;
use crate::common::{self, *};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // pointer to ... type
    TyPtr(Box<Type>),
    TyInt,
    TyChar,
    ArrayOf(Box<Type>, i32),
    // function return ... type
    TyFunc(Box<Type>),
    ty_none,
}
use Type::*;

pub fn sizeof(ty: &Type) -> i32 {
    match ty {
        TyPtr(_) => 8,
        TyInt => 8,
        TyChar => 1,
        ArrayOf(element_ty, len) => sizeof(element_ty) * len,
        TyFunc(_) => 8,
        ty_none => 8,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Obj {
    pub name: String,
    pub ty: Type,
    // this offset should be based on %rbp
    pub offset: i32,
    pub is_global: bool,
    // @TODO: Add position info.
    // When a variable is already defined, the compiler should tell where the variable is defined.
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub objects: Vec<Obj>,
}

impl Scope {
    pub fn new() -> Self {
        Scope{objects: Vec::new()}
    }

    // @TODO: search upwards
    pub fn resolve_symbol(&self, s: &str) -> Option<&Obj> {
        for o in &self.objects {
            if o.name == s {
                return Some(o);
            }
        }
        None
    }

    pub fn add_obj(&mut self, o: Obj) {
        self.objects.push(o);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeTracker {
    pub scopes: Vec<Scope>,
    pub current_scope_index: usize,
}

impl ScopeTracker {
    pub fn new(base_scope: &Scope) -> Self {
        let mut scopes: Vec<Scope> = Vec::new();
        scopes.push(base_scope.clone());
        ScopeTracker{scopes, current_scope_index: 0}
    }

    pub fn enter_new_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.current_scope_index += 1;
    }

    pub fn resolve_symbol(&self, s: &str) -> Option<&Obj> {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if let Some(o) = current_scope.resolve_symbol(s) {
                return Some(o)
            }
        }
        return None
    }

    pub fn add_obj(&mut self, o: Obj) {
        let current_scope = &mut self.scopes[self.current_scope_index];
        current_scope.add_obj(o);
    }

    pub fn add_private_global_obj(&mut self, o: Obj) {
        let current_scope = &mut self.scopes[0];
        current_scope.add_obj(o);
    }
}

pub struct ProgramAnalyzer {
    pub global_scope: Scope,
    pub global_decls: Vec<ir::Declaration>,
    // At the start of each function analyzation, the folloing two fields are cleared
    pub cur_scope_tracker: ScopeTracker,
    pub cur_offset: i32,
    pub unique_string_name_index: i32,
}

impl ProgramAnalyzer {
    pub fn new() -> Self {
        let scope = Scope::new();
        ProgramAnalyzer{
                        global_scope: scope.clone(),
                        global_decls: Vec::new(),
                        cur_scope_tracker: ScopeTracker::new(&scope),
                        cur_offset: 0,
                        unique_string_name_index: 0,
                        }
    }

    pub fn analyze(&mut self, mut program: Program) -> ir::AnalyzedProgram {
        use ir::Function;
        let mut afuns: Vec<ir::Function> = Vec::new();
        let mut a_global_decls: Vec<ir::Declaration> = Vec::new();
        for unit in program.translation_units {
            match unit {
                parse::TranslationUnit::FunctionDef(fun) => {
                    self.cur_scope_tracker = ScopeTracker::new(&self.global_scope);
                    let afun = self.analyze_function(fun);
                    afuns.push(afun);
                }
                parse::TranslationUnit::GlobalDecl(decl) => {
                    let mut batch_global_decls = self.analyze_global_decl(decl);
                    self.global_decls.append(&mut batch_global_decls);
                }
            }
        }
        ir::AnalyzedProgram{afuns, global_decls: self.global_decls.clone()}
    }

    pub fn analyze_global_decl(&mut self, mut decl: Declaration) -> Vec::<ir::Declaration> {
        let mut decls: Vec<ir::Declaration> = Vec::new();
        let base_type: Type;
        match &decl.decl_spec {
            SpecInt => base_type = TyInt,
            SpecChar => base_type = TyChar,
        }
        for init in &mut decl.init_declarators {
            if let Some(_) = self.global_scope.resolve_symbol(&init.declarator.name) {
                let err_info = format!("global variable {} already defined", init.declarator.name);
                // TODO: error handling
                print_error_at(init.declarator.span, &err_info);
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
            if let Some(expr) = &mut init.init_expr {
                let analyzed_expr = self.analyze_expr(expr);
                // @Future: Currently, we only support constant number assignment.
                // We will add array and struct initialization expr assignment in the future.
                match analyzed_expr.content {
                    ir::ExprType::Number(n) => {
                        if !can_assign(&cur_type, &analyzed_expr.ty) {
                            let err_info = format!("mismatch types: {} type is {:?}, but expression type is {:?}",
                            init.declarator.name, &cur_type, &analyzed_expr.ty);
                            print_error_at(init.declarator.span, &err_info);
                        } else {
                            let object = create_global_obj(&init.declarator.name, &cur_type);
                            let value_in_bytes = n.to_le_bytes();
                            let analyzed_decl = ir::Declaration{obj: object.clone(), init_value: Some(value_in_bytes.to_vec())};
                            decls.push(analyzed_decl);
                            self.global_scope.add_obj(object);
                        }
                    }
                    _ => {
                        let err_info = format!("This is not a constant number expression!");
                        print_error_at(analyzed_expr.span, &err_info);
                    }
                }
            } else { // decl without initializer
                let object = create_global_obj(&init.declarator.name, &cur_type);
                let analyzed_decl = ir::Declaration{obj: object.clone(), init_value: None};
                decls.push(analyzed_decl);
                self.global_scope.add_obj(object);
            }
        }
        decls
    }

    pub fn analyze_function(&mut self, mut fun: Function) -> ir::Function {
        self.cur_scope_tracker.enter_new_scope();
        let base = match &fun.return_type {
            SpecInt => TyInt,
            SpecChar => TyChar,
        };
        let mut return_type = base;
        for i in 0..fun.star_count {
            return_type = pointer_to(&return_type);
        }
        let name = fun.name;

        // Add function name to symbol table
        // @Fix: We should create obj after we are sure that the variable is not defined yet.
        let obj = self.create_obj(&return_type, &name);
        if self.cur_scope_tracker.resolve_symbol(&obj.name) == None {
            self.cur_scope_tracker.add_obj(obj);
        } else {
            let err_info = format!("fatal error: parameter variable {} already defined", obj.name);
            self.print_error_at(fun.name_span, &err_info);
        }

        let mut param_names: Vec<String> = Vec::new();
        for param in &fun.params {
            self.analyze_param(param);
            param_names.push(param.declarator.name.clone());
        }
        let mut stmts: Vec<ir::StmtType> = Vec::new();
        for item in &mut fun.items {
            match item {
                Stmt(stmt) => {
                    let ir_stmt = self.analyze_stmt(stmt);
                    stmts.push(ir_stmt);
                }
                Decl(decl) => {
                    let mut ir_stmts = self.analyze_decl(decl);
                    stmts.append(&mut ir_stmts);
                }
            }
        }
        let scope_tracker = self.cur_scope_tracker.clone();
        let stack_size = self.cur_offset;
        ir::Function{name, return_type, param_names, stmts, scope_tracker, stack_size}
    }

    fn analyze_items(&mut self, items: &mut Vec<BlockItem>) -> Vec<ir::StmtType> {
        let mut stmts: Vec<ir::StmtType> = Vec::new();
        for item in items {
            match item {
                Stmt(stmt) => stmts.push(self.analyze_stmt(stmt)),
                Decl(decl) => stmts.append(&mut self.analyze_decl(decl)),
            }
        }
        stmts
    }


    fn analyze_param(&mut self, param: &Parameter) {
        let base_type: Type;
        match &param.decl_spec {
            SpecInt => base_type = TyInt,
            SpecChar => base_type = TyChar,
        }
        let obj = self.create_obj(&base_type, &param.declarator.name);
        if self.cur_scope_tracker.resolve_symbol(&obj.name) == None {
            self.cur_scope_tracker.add_obj(obj);
        } else {
            let err_info = format!("fatal error: parameter variable {} already defined", obj.name);
            self.print_error_at(param.declarator.span, &err_info);
        }
    }


    // after analyze, declarations are all resolved to creating obj and assignment statement.
    fn analyze_decl(&mut self, decl: &mut Declaration) -> Vec<ir::StmtType> {
        let mut stmts: Vec<ir::StmtType> = Vec::new();
        let base_type: Type;
        match &decl.decl_spec {
            SpecInt => base_type = TyInt,
            SpecChar => base_type = TyChar,
        }
        for init in &mut decl.init_declarators {
            if let Some(_) = self.cur_scope_tracker.resolve_symbol(&init.declarator.name) {
                let err_info = format!("variable {} already defined", init.declarator.name);
                self.print_error_at(init.declarator.span, &err_info);
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
            self.cur_scope_tracker.add_obj(obj.clone());
            if let Some(expr) = &mut init.init_expr {
                let analyzed_expr = self.analyze_expr(expr);
                if !can_assign(&obj.ty, &analyzed_expr.ty) {
                    let err_info = format!("mismatch types: {} type is {:?}, but expression type is {:?}",
                    obj.name, &obj.ty, &analyzed_expr.ty);
                    self.print_error_at(init.declarator.span, &err_info);
                } else {
                    let expr = self.gen_expr_from_obj(&obj);
                    let content = ir::ExprType::Assign(Box::new(expr), Box::new(analyzed_expr));
                    let generated_expr = ir::Expr{content, ty: obj.ty, span: init.declarator.span};
                    let generated_stmt = ir::StmtType::Ex(generated_expr);
                    // if let ir::StmtType::Ex{..} = generated_stmt {
                    //     println!("aaaaaaaaaaaaaaaaaaaaaaaaaaaa");
                    // }
                    stmts.push(generated_stmt);
                }
            }
        }
        stmts
    }

    fn gen_expr_from_obj(&self, o: &Obj) -> ir::Expr {
        let content = ir::ExprType::Ident(o.name.clone());
        let span = Span{start_index: 0, end_index: 0};
        ir::Expr{content, ty: o.ty.clone(), span}
        
    }


    // @Naming: Rename it to create_local_obj.
    fn create_obj(&mut self, base_type: &Type, name: &str) -> Obj {
        let mut cur_type = base_type.clone();
        let mut size: i32 = sizeof(base_type);
        let obj = Obj{name: name.to_string(), ty: cur_type, offset: self.cur_offset, is_global: false};
        // @Smell: This line should be executed outside of this function.
        self.cur_offset += size;
        obj
    }

    fn analyze_stmt(&mut self, stmt: &mut StmtType) -> ir::StmtType {
        use ir::StmtType;
        match stmt {
            Ex(expr) => {
                let expr = self.analyze_expr(expr);
                StmtType::Ex(expr)
            },
            Return(expr) => {
                let expr = self.analyze_expr(expr);
                StmtType::Return(expr)
            },
            Block(items) => {
                let stmts = self.analyze_items(items);
                StmtType::Block(stmts)
            }
            If(parse::IfStmt{cond, then, otherwise}) => {
                let cond = self.analyze_expr(cond);
                let then = self.analyze_stmt(then);
                let then = Box::new(then);
                let otherwise = if let Some(otherwise) = otherwise {
                    Some(Box::new(self.analyze_stmt(otherwise)))
                } else {
                    None
                };
                StmtType::If{cond, then, otherwise}
            }
            For(parse::ForStmt{init, cond, inc, then}) => {
                let init = if let Some(init) = init {
                    let init = self.analyze_expr(init);
                    Some(init)
                } else {
                    None
                };
                let cond = if let Some(cond) = cond {
                    let cond = self.analyze_expr(cond);
                    Some(cond)
                } else {
                    None
                };
                let inc = if let Some(inc) = inc {
                    let inc = self.analyze_expr(inc);
                    Some(inc)
                } else {
                    None
                };
                let then = Box::new(self.analyze_stmt(then));
                StmtType::For{init, cond, inc, then}
            }
        }
    }

    fn analyze_expr(&mut self, expr: &mut Expr) -> ir::Expr {
        use ir::ExprType;
        use ir::OP;
        let span = expr.span;
        match &mut expr.content {
            Number(n) => {
                let content = ExprType::Number(*n);
                let ty = TyInt;
                ir::Expr {content, ty, span}
            }
            Binary(lhs, rhs, tokenKind) => {
                let mut lhs = self.analyze_expr(lhs);
                let mut rhs = self.analyze_expr(rhs);
                match tokenKind {
                    // deal with pointer arithmatic
                    Plus => {
                        if lhs.is_ptr() && rhs.is_ptr() {
                            println!();
                            self.print_error_at(lhs.span, "error: both lhs and rhs are of ptr type");
                            self.print_error_at(rhs.span, "error: both lhs and rhs are of ptr type");
                        }
                        if lhs.is_integer() && rhs.is_ptr() {
                            swap(&mut lhs, &mut rhs);
                        }
                        if lhs.is_ptr() && rhs.is_integer() {
                            let mut scal: i32;
                            match &lhs.ty {
                                TyPtr(..) => scal = sizeof(&lhs.ty),
                                ArrayOf(element_type, _) => scal = sizeof(element_type),
                                _ => scal = 8,
                            }
                            rhs = scal_expr(&mut rhs, Mul, scal);
                        }
                        let ty = lhs.ty.clone();
                        let content = ExprType::Binary(Box::new(lhs), Box::new(rhs), OP::Plus);
                        ir::Expr {content, ty, span}
                    }
                    Minus => {
                        if lhs.is_integer() && rhs.is_ptr() {
                            self.print_error_at(rhs.span, "error: integer - ptr");
                        }
                        if is_pointer_or_array(&lhs.ty) && rhs.is_integer() {
                            let mut scal: i32;
                            match &lhs.ty {
                                TyPtr(..) => scal = sizeof(&lhs.ty),
                                ArrayOf(element_type, _) => scal = sizeof(element_type),
                                _ => scal = 8,
                            }
                            rhs = scal_expr(&rhs, Mul, scal);
                        } else if is_pointer_or_array(&lhs.ty) && is_pointer_or_array(&rhs.ty) {
                            let mut basic_ty = lhs.ty.clone();
							if let ArrayOf(basic, _) = &lhs.ty {
								basic_ty = *basic.clone();
							} else if let TyPtr(basic) = &lhs.ty {
								basic_ty = *basic.clone();
                            }
							if lhs.ty != rhs.ty {
								self.print_error_at(rhs.span, "pointer arithmatic warning: type doesn't match");
							}
                            let ty = TyInt;
                            let content = ExprType::Binary(Box::new(lhs), Box::new(rhs), OP::Minus);
                            let expr = ir::Expr {content, ty, span};
                            let scal = sizeof(&basic_ty);
                            return scal_expr(&expr, Div, scal);
                        }
                        let ty = lhs.ty.clone();
                        let content = ExprType::Binary(Box::new(lhs), Box::new(rhs), OP::Minus);
                        ir::Expr {content, ty, span}
                    }
                    _ => {
                        let op = tokenkind_to_op(tokenKind);
                        let ty = lhs.ty.clone();
                        let content = ExprType::Binary(Box::new(lhs), Box::new(rhs), op);
                        ir::Expr {content, ty, span}
                    }
                }
            }
            Assign(lhs, rhs) => {
                let rhs = self.analyze_expr(rhs);
                let lhs = self.analyze_expr(lhs);
                if !can_be_lvalue(&lhs) {
                    // @Incomplete: more precise error report: the reason why lhs cannot be lvalue
                    let err_info = format!("lhs cannot be lvalue: {:?}", &lhs.ty);
                    self.print_error_at(lhs.span, &err_info);
                }
                if !can_assign(&lhs.ty, &rhs.ty) {
                    let err_info = format!("mismatch types: try to assign type {:?} to type {:?}",
                    &rhs.ty, &lhs.ty);
                    self.print_error_at(lhs.span, &err_info);
                }
                let ty = lhs.ty.clone();
                let content = ExprType::Assign(Box::new(lhs), Box::new(rhs));
                ir::Expr{content, ty, span}
            }
            Neg(val) => {
                let val = self.analyze_expr(val);
                let ty = val.ty.clone();
                let content = ExprType::Neg(Box::new(val));
                ir::Expr{content, ty, span}
            }
            Deref(val) => {
                let val = self.analyze_expr(val);
                let base_ty = match &val.ty {
                    TyPtr(base) => {
                        *base.clone()
                    }
                    ArrayOf(base, _) => {
                        *base.clone()
                    }
                    _ => {
                        let err_msg = format!("semantic error: invalid dereferencing:
                        try to dereference {:?}", val.ty);
                        self.print_error_at(expr.span, &err_msg);
                        val.ty.clone()
                    }
                };
                let content = ExprType::Deref(Box::new(val));
                ir::Expr{content, ty: base_ty, span}
            }
            AddrOf(val) => {
                let val = self.analyze_expr(val);
                let ty = pointer_to(&val.ty);
                let content = ExprType::AddrOf(Box::new(val));
                ir::Expr{content, ty, span}
            }
            Ident(s) => {
                let ty = if let Some(o) = self.cur_scope_tracker.resolve_symbol(s) {
                    o.ty.clone()
                } else {
                    let err_info = format!("semantic error: symbol '{}' not found", s);
                    self.print_error_at(expr.span, &err_info);
                    TyInt
                };
                let content = ExprType::Ident(s.clone());
                ir::Expr{content, ty, span}
            }
            ArrayIndexing(arr_ref, indices) => {
                let mut analyzed_indices = Vec::new();
                let mut arr_ref = self.analyze_expr(arr_ref);
                             // reborrow here
                for index in &mut *indices {
                    let analyzed_index = self.analyze_expr(index);
                    analyzed_indices.push(analyzed_index);
                }
                let cur_ref = &mut arr_ref;
                for index in analyzed_indices {
                    // type checking
                    if !cur_ref.is_ptr() {
                        self.print_error_at(index.span, "subscripted value is neither array nor pointer nor vector");
                    } else {
                        // Array indexing is converted to pointer arithmatic and dereferencing
                        // pointer arithmatic.
                        let size = match &cur_ref.ty {
                                TyPtr(..) => sizeof(&cur_ref.ty),
                                ArrayOf(element_type, _) => sizeof(element_type),
                                _ => 8,
                            };
                        let scaled = scal_expr(&index, Mul, size);
                        let pointer_arithmatic = ExprType::Binary(Box::new(cur_ref.clone()), Box::new(scaled), OP::Plus);
                        let mut pointer_arithmatic_expr = ir::Expr {
                            content: pointer_arithmatic,
                            ty: cur_ref.ty.clone(),
                            span,
                        };

                        // Dereferencing.
                        let base_ty = match &pointer_arithmatic_expr.ty {
                            TyPtr(base) => {
                                *base.clone()
                            }
                            ArrayOf(base, _) => {
                                *base.clone()
                            }
                            _ => {
                                let err_msg = format!("semantic error: invalid dereferencing:
                                try to dereference {:?}", pointer_arithmatic_expr.ty);
                                self.print_error_at(expr.span, &err_msg);
                                cur_ref.ty.clone()
                            }
                        };

                        let deref = ExprType::Deref(Box::new(pointer_arithmatic_expr));
                        let mut deref_expr = ir::Expr {
                            content: deref,
                            ty: base_ty,
                            span,
                        };
                        *cur_ref = deref_expr;
                    }
                }
                (*cur_ref).clone()
            }
            FunCall(ident, args) => {
                // @Fix: The problem is that analyze_expr(ident) will check whether ident is declared and if not declared,
                // that's an error, but the function name is intentionally to be undeclared. 
                // @Future: The above problem will be solved when we add function declaration. At that time every valid function call can
                // find its declaration without adding obj at here in the following code (because the obj is added after the compiler
                // dealt with the corresponding declaration).
                match &ident.content {
                    Ident(s) => {
                        let obj = self.create_obj(&ty_none, &s);
                        self.cur_scope_tracker.add_obj(obj.clone());
                    }
                    _ => println!("currently only support function name as call reference"),
                }
                let ident = self.analyze_expr(ident);
                let mut analyzed_args = Vec::new();
                for arg in args {
                    let analyzed_arg = self.analyze_expr(arg);
                    analyzed_args.push(analyzed_arg);
                }
                // The function name may be in another elf file, we don't check its validaity. (@Future: Not true after we add function declaration)
                let ty = ident.ty.clone();
                let content = ExprType::FunCall(Box::new(ident), analyzed_args);
                ir::Expr {
                    content,
                    ty,
                    span,
                }
            }
            // @Temp: We only consider compile time sizeof for now
            Sizeof(expr_content) => {
                let content = self.analyze_expr(expr_content);
                let size = sizeof(&content.ty);
                let ty = content.ty.clone();
                let content = ExprType::Number(size);
                ir::Expr {
                    content,
                    ty,
                    span,
                }
            }
            Str(s) => {
                // We use a unique identifier as a reference to replace the original string literal.
                // The string literal shall be initialized in .data section.
                let unique_name = format!(".LC{}", self.unique_string_name_index);
                self.unique_string_name_index += 1;
                // In C, a string ends with an extra \0 character, so the array length +1.
                // Note: we use s.len() rather than the source span because parse_paren may
                // overwrite the literal's span to cover surrounding parentheses (e.g. sizeof("")).
                let len = s.len() + 1;
                let ty: Type = ArrayOf(Box::new(TyChar), len.try_into().unwrap());


                let global_obj = create_global_obj(&unique_name, &ty);
                self.cur_scope_tracker.add_private_global_obj(global_obj.clone());
                // self.global_scope.add_obj(object);
                let mut value_in_bytes = s.clone();
                value_in_bytes.push(b'\0');
                let global_decl = ir::Declaration{obj: global_obj.clone(), init_value: Some(value_in_bytes)};
                self.global_decls.push(global_decl);

                let unique_symbol = ExprType::Ident(unique_name);
                ir::Expr{content: unique_symbol, ty, span}
            }
            Paren(inner) => self.analyze_expr(inner),
            StmtExpr(items) => {
                let stmts = self.analyze_items(items);
                let ty = match stmts.last() {
                    Some(ir::StmtType::Ex(e)) => e.ty.clone(),
                    _ => {
                        self.print_error_at(span, "a statement expression must end with an expression statement");
                        ty_none
                    }
                };
                let content = ExprType::StmtExpr(stmts);
                ir::Expr{content, ty, span}
            },
        }
    }

    fn error_expr(&self, expr: &Expr, info: &str) -> String {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(expr.span.start_index);
        let arrows = "^".repeat(expr.span.end_index - expr.span.start_index + 1);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        err_msg
    }

    fn print_error_at(&self, span: Span, info: &str) {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(span.start_index);
        let arrows = "^".repeat(span.end_index - span.start_index + 1);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        println!("{}", err_msg);
    }

    fn print_error_expr(&self, expr: &Expr, info: &str) {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(expr.span.start_index);
        let arrows = "^".repeat(expr.span.end_index - expr.span.start_index + 1);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        println!("{}", err_msg);
    }

    fn print_error_ir_expr(&self, expr: &ir::Expr, info: &str) {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(expr.span.start_index);
        let arrows = "^".repeat(expr.span.end_index - expr.span.start_index + 1);
        err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
        println!("{}", err_msg);
    }

    fn err_declarator(&self, declarator: &Declarator, info: &str) -> String {
        let mut err_msg = String::from("");
        let src_str: &str = &SRC.lock().unwrap().to_string();
        err_msg.push_str(&format!("{}\n", src_str));
        let spaces = " ".repeat(declarator.span.start_index);
        let arrows = "^".repeat(declarator.span.end_index - declarator.span.start_index + 1);
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

fn dimension_of(ty: &Type) -> i32 {
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

// evaluate whether a expression of right type can be assigned to a "stuff"
// of left type
fn is_integer(ty: &Type) -> bool {
    matches!(ty, TyInt | TyChar)
}

fn can_assign(left: &Type, right: &Type) -> bool {
    // array can be assigned to a pointer type, BUT not the other way around!
    if is_pointer(left) && is_pointer_or_array(right) {
        return true
    } else if is_integer(left) && is_integer(right) {
        return true
    } else {
        return left == right
    }
}


fn can_be_lvalue(expr: &ir::Expr) -> bool {
    use ir::ExprType;
    match expr.content {
        ExprType::FunCall(_, _) => false,
        ExprType::Ident(_) => {
            if let ArrayOf(_, _) = expr.ty {
                false
            } else {
                true
            }
        }
		_ => true,
    }
}

fn can_be_rvalue(expr: &Expr) -> bool {
    return true;
}

pub fn is_pointer_or_array(t: &Type) -> bool {
    match t {
        TyPtr(_) | ArrayOf(_, _) => true,
        _ => false
    }
}

pub fn is_pointer(t: &Type) -> bool {
    match t {
        TyPtr(_) => true,
        _ => false
    }
}

pub fn is_array(t: &Type) -> bool {
    match t {
        ArrayOf(_, _) => true,
        _ => false
    }
}

fn scal_expr(expr: &ir::Expr, operation: TokenKind, scal: i32) -> ir::Expr {
    use ir::OP;
    // expr for scal num
    let num_expr_type = ir::ExprType::Number(scal);
    let num_expr = ir::Expr {
        content: num_expr_type,
        ty: TyInt,
        span: expr.span
    };

    // scalled expr
    let op = tokenkind_to_op(&operation);
    let new_expr_type = ir::ExprType::Binary(Box::new(expr.clone()), Box::new(num_expr), op);
    ir::Expr {
        content: new_expr_type,
        ty: expr.ty.clone(), 
        span: expr.span,
    }
}

fn tokenkind_to_op(tokenkind: &TokenKind) -> ir::OP {
    use ir::OP;
    match tokenkind {
        Plus => OP::Plus,
        Minus => OP::Minus,
        Mul => OP::Mul,
        Div => OP::Div,
        Compare(Eq) => OP::Compare(ir::CompareToken::Eq),
        Compare(Neq) => OP::Compare(ir::CompareToken::Neq),
        Compare(LT) => OP::Compare(ir::CompareToken::LT),
        Compare(LE) => OP::Compare(ir::CompareToken::LE),
        Compare(GT) => OP::Compare(ir::CompareToken::GT),
        Compare(GE) => OP::Compare(ir::CompareToken::GE),
        // @Cleanup: Binary operation should be defined at parsing phase.
        // Currently the operation is just tokenkind in parsing.
        _ => OP::Compare(ir::CompareToken::Eq),
    }
}

fn to_ir_ident(name: &str, ty: &Type, span: &Span) -> ir::Expr {
    ir::Expr {
        content: ir::ExprType::Ident(name.to_string()),
        ty: ty.clone(),
        span: span.clone(),
    }
}

fn create_global_obj(name: &str, base_type: &Type) -> Obj {
    let mut cur_type = base_type.clone();
    let mut size: i32 = sizeof(base_type);
    let obj = Obj{name: name.to_string(), ty: base_type.clone(), offset: 0, is_global: true};
    // @Smell: This line should be executed outside of this function.
    // self.cur_offset += size;
    obj
}

fn print_error_at(span: Span, info: &str) {
    let mut err_msg = String::from("");
    let src_str: &str = &SRC.lock().unwrap().to_string();
    err_msg.push_str(&format!("{}\n", src_str));
    let spaces = " ".repeat(span.start_index);
    let arrows = "^".repeat(span.end_index - span.start_index + 1);
    err_msg.push_str(&format!("{}{} {}", spaces, arrows.red(), info.red()));
    println!("{}", err_msg);
}