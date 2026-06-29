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
use TypeSpec::*;
use crate::SRC;
use crate::common::{self, *};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Pointer_To(Box<Type>),
    Int,
    Char,
    ArrayOf(Box<Type>, i32),
    // function return ... type
    Func(Box<Type>),
    Struct(ir::Struct),
    Tagged_Struct(String),
    ty_none,
}
use Type::*;

impl Type {
    fn align(&self) -> i32 {
        match self {
            Pointer_To(_) => 8,
            Type::Int => 8,
            Type::Char => 1,
            ArrayOf(element_ty, len) => element_ty.align(),
            Func(_) => 8,
            Struct(st) => st.align,
            Tagged_Struct(_) => 0,
            ty_none => 1,
        }
    }
}

pub fn sizeof(ty: &Type) -> i32 {
    match ty {
        Pointer_To(_) => 8,
        Type::Int => 8,
        Type::Char => 1,
        ArrayOf(element_ty, len) => sizeof(element_ty) * len,
        Func(_) => 8,
        Struct(st) => {
            st.size
        },
        Tagged_Struct(tag_name) => {
            // @Refactor: Return an error.
            let error_info = format!("unable to get the size of incomplete type");
            println!("{}", error_info);
            exit(1);
        },
        // @TODO This should return 0.
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
    pub tags: Vec<ir::Tagged_Struct>,
}

impl Scope {
    pub fn new() -> Self {
        Scope{
            objects: Vec::new(),
            tags: Vec::new(),
        }
    }

    pub fn resolve_symbol(&self, s: &str) -> Option<&Obj> {
        for o in &self.objects {
            if o.name == s {
                return Some(o);
            }
        }
        None
    }

    pub fn resolve_struct_tag(&self, s: &str) -> Option<&ir::Struct> {
        for st in &self.tags {
            if &st.tag_name == s {
                    return Some(&st.the_struct);
            }
        }
        None
    }

    pub fn add_obj(&mut self, o: Obj) {
        self.objects.push(o);
    }

    pub fn add_tagged_struct(&mut self, tagged: ir::Tagged_Struct) {
        debug_assert!(self.resolve_struct_tag(&tagged.tag_name) == None);
        for st in &mut self.tags {
            if &st.tag_name == &tagged.tag_name {
                st.the_struct = tagged.the_struct;
                return;
            }
        }
        self.tags.push(tagged);
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

    pub fn exit_current_scope(&mut self) {
        self.scopes.pop();
        self.current_scope_index -= 1;
    }

    pub fn resolve_tag(&self, s: &str) -> Option<&ir::Struct> {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if let Some(st) = current_scope.resolve_struct_tag(s) {
                return Some(st)
            }
        }
        return None
    }

    pub fn resolve_tag_at_current_scope(&self, s: &str) -> Option<&ir::Struct> {
        let current_scope = &self.scopes[self.current_scope_index];
        return current_scope.resolve_struct_tag(s);
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

    pub fn resolve_symbol_at_current_scope(&self, s: &str) -> Option<&Obj> {
        let current_scope = &self.scopes[self.current_scope_index];
        return current_scope.resolve_symbol(s);
    }

    pub fn add_obj(&mut self, o: Obj) {
        let current_scope = &mut self.scopes[self.current_scope_index];
        current_scope.add_obj(o);
    }

    pub fn add_tagged_struct(&mut self, tagged: ir::Tagged_Struct) {
        let current_scope = &mut self.scopes[self.current_scope_index];
        current_scope.add_tagged_struct(tagged);
    }

    pub fn add_private_global_obj(&mut self, o: Obj) {
        let global_scope_in_current_function = &mut self.scopes[0];
        global_scope_in_current_function.add_obj(o);
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
        for unit in &mut program.translation_units {
            match unit {
                parse::TranslationUnit::FunctionDef(fun) => {
                    if self.global_scope.resolve_symbol(&fun.name) == None {
                        let ty = self.analyze_decl_spec(&fun.return_type);
                        let o = create_global_obj(&fun.name, &ty);
                        self.global_scope.add_obj(o);
                    } else {
                        let err_info = format!("semantic error: redefinition of {}", fun.name);
                        print_error_at(fun.name_span, &err_info);
                    }
                }
                parse::TranslationUnit::GlobalDecl(decl) => {
                    let mut batch_global_decls = self.analyze_global_decl(decl);
                    self.global_decls.append(&mut batch_global_decls);
                }
            }
        }
        for unit in &mut program.translation_units {
            if let parse::TranslationUnit::FunctionDef(fun) = unit {
                self.cur_offset = 0;
                self.cur_scope_tracker = ScopeTracker::new(&self.global_scope);
                self.cur_scope_tracker.enter_new_scope();
                let afun = self.analyze_function(fun);
                afuns.push(afun);
            }
        }
        ir::AnalyzedProgram{afuns, global_decls: self.global_decls.clone()}
    }

    pub fn analyze_global_decl(&mut self, decl: &mut Declaration) -> Vec::<ir::Declaration> {
        let mut decls: Vec<ir::Declaration> = Vec::new();
        let base_type = self.analyze_decl_spec(&decl.decl_spec);
        for declarator in &mut decl.declarators {
            if let Some(_) = self.global_scope.resolve_symbol(&declarator.name) {
                let err_info = format!("global variable {} already defined", declarator.name);
                // TODO: error handling
                print_error_at(declarator.span, &err_info);
            }
            // deal with pointers
            let mut cur_type = base_type.clone();
            for i in 0..declarator.star_count {
                cur_type = pointer_to(&cur_type);
            }
            // deal with suffix
            if let Some(suffix) = &mut declarator.suffix {
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
            if let Some(expr) = &mut declarator.init_expr {
                let analyzed_expr = self.analyze_expr(expr);
                // @Future: Currently, we only support constant number assignment.
                // We will add array and struct initialization expr assignment in the future.
                match analyzed_expr.content {
                    ir::ExprType::Number(n) => {
                        if !can_assign(&cur_type, &analyzed_expr.ty) {
                            let err_info = format!("mismatch types: {} type is {:?}, but expression type is {:?}",
                            declarator.name, &cur_type, &analyzed_expr.ty);
                            print_error_at(declarator.span, &err_info);
                        } else {
                            let object = create_global_obj(&declarator.name, &cur_type);
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
                let object = create_global_obj(&declarator.name, &cur_type);
                let analyzed_decl = ir::Declaration{obj: object.clone(), init_value: None};
                decls.push(analyzed_decl);
                self.global_scope.add_obj(object);
            }
        }
        decls
    }

    pub fn analyze_function(&mut self, fun: &mut Function) -> ir::Function {
        let base_type = self.analyze_decl_spec(&fun.return_type);
        let mut return_type = base_type;
        for i in 0..fun.star_count {
            return_type = pointer_to(&return_type);
        }
        let name = fun.name.clone();
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
        let base_type = self.analyze_decl_spec(&param.decl_spec);
        let mut cur_type = base_type.clone();
        for _ in 0..param.declarator.star_count {
            cur_type = pointer_to(&cur_type);
        }
        let obj = self.create_obj(&cur_type, &param.declarator.name);
        if self.cur_scope_tracker.resolve_symbol(&obj.name) == None {
            self.cur_scope_tracker.add_obj(obj);
        } else {
            let err_info = format!("fatal error: parameter variable {} already defined", obj.name);
            print_error_at(param.declarator.span, &err_info);
        }
    }


    // After analyzation, declarations are all resolved to creating obj and assignment statement.
    fn analyze_decl(&mut self, decl: &mut Declaration) -> Vec<ir::StmtType> {
        let mut stmts: Vec<ir::StmtType> = Vec::new();
        let base_type = self.analyze_decl_spec(&decl.decl_spec);
        for declarator in &mut decl.declarators {
            if let Some(_) = self.cur_scope_tracker.resolve_symbol_at_current_scope(&declarator.name) {
                let err_info = format!("variable {} already defined", declarator.name);
                print_error_at(declarator.span, &err_info);
            }
            // deal with pointers
            let mut cur_type = base_type.clone();
            for i in 0..declarator.star_count {
                cur_type = pointer_to(&cur_type);
            }
            // deal with suffix
            if let Some(suffix) = &declarator.suffix {
                match suffix {
                    DeclaratorSuffix::ArrayLen(lens) => {
                        for len in lens.iter().rev() {
                            cur_type = array_of(&cur_type, *len);
                        }
                    }
                    DeclaratorSuffix::FunParam(_) => todo!(),
                }
            }

            if let Tagged_Struct(tag_name) = cur_type.clone() {
                match self.cur_scope_tracker.resolve_tag(&tag_name) {
                    Some(attribute) => cur_type = Struct(attribute.clone()),
                    None => {
                        let err_info = format!("storage size of {} is unkonwn", &declarator.name);
                        print_error_at(declarator.span, &err_info);
                        continue;
                    },
                }
            }

            let obj = self.create_obj(&cur_type, &declarator.name);
            self.cur_scope_tracker.add_obj(obj.clone());
            if let Some(expr) = &mut declarator.init_expr {
                let analyzed_expr = self.analyze_expr(expr);
                if !can_assign(&obj.ty, &analyzed_expr.ty) {
                    let err_info = format!("mismatch types: {} type is {:?}, but expression type is {:?}",
                    obj.name, &obj.ty, &analyzed_expr.ty);
                    print_error_at(declarator.span, &err_info);
                } else {
                    let expr = self.gen_expr_from_obj(&obj);
                    let content = ir::ExprType::Assign(Box::new(expr), Box::new(analyzed_expr));
                    let generated_expr = ir::Expr{content, ty: obj.ty, span: declarator.span};
                    let generated_stmt = ir::StmtType::Ex(generated_expr);
                    stmts.push(generated_stmt);
                }
            }
        }
        stmts
    }

    fn analyze_decl_spec(&mut self, decl_spec: &TypeSpec) -> Type {
        match decl_spec {
            TypeSpec::Int => Type::Int,
            TypeSpec::Char => Type::Char,
            TypeSpec::SpecStruct(st) => self.analyze_struct(st),
        }
    }

    fn analyze_struct(&mut self, st: &StructSpecifier) -> Type {
        if let Some(name) = &st.name {
            if let Some(members) = &st.members {
                let struct_spec = ir::Tagged_Struct{
                    tag_name: name.clone(),
                    the_struct: self.analyze_struct_members(members),
                };
                if let None = self.cur_scope_tracker.resolve_tag_at_current_scope(name) {
                    self.cur_scope_tracker.add_tagged_struct(struct_spec);
                } else {
                    let err_info = format!("semantic error: redefinition of struct tag name: '{}'", name);
                    // @TODO add span info to declaration specifier
                    print_error_at(Span{start_index: 0, end_index: 0}, &err_info);
                }
            }
            return Tagged_Struct(name.clone());
        } else {
            if let Some(members) = &st.members {
                    let attribute = self.analyze_struct_members(members);
                    return Struct(attribute);
            } else {
                let err_info = format!("semantic error: analyzing strcut decl: both identifier and decl list are empty.");
                // @TODO add span info to declaration specifier
                print_error_at(Span{start_index: 0, end_index: 0}, &err_info);
                // @Refactor: We return an error here.
                return ty_none;
            }
        }
    }

    fn analyze_struct_members(&mut self, members: &Vec<Member>) -> ir::Struct {
        let mut analyzed_members = Vec::new();
        let mut offset: i32 = 0;
        let mut struct_align: i32 = 1;
        for m in members {
            let mut am = self.analyze_struct_member(m, offset);
            let member_align = am.ty.align();
            offset = align_to(offset, member_align);
            am.offset = offset;
            offset += sizeof(&am.ty);
            if struct_align < member_align {
                struct_align = member_align;
            }
            analyzed_members.push(am);
        }
        let struct_size = align_to(offset, struct_align);
        return ir::Struct{
            members: analyzed_members,
            size: struct_size,
            align: struct_align,
        };
    }

    fn analyze_struct_member(&mut self, member: &Member, offset: i32) -> ir::Member {
        let base_ty = self.analyze_decl_spec(&member.decl_spec);
        // deal with pointers
        let mut cur_type = base_ty.clone();
        for i in 0..member.declarator.star_count {
            cur_type = pointer_to(&cur_type);
        }
        // deal with suffix
        if let Some(suffix) = &member.declarator.suffix {
            match suffix {
                DeclaratorSuffix::ArrayLen(lens) => {
                    for len in lens.iter().rev() {
                        cur_type = array_of(&cur_type, *len);
                    }
                }
                DeclaratorSuffix::FunParam(_) => todo!(),
            }
        }
        ir::Member{
            ty: cur_type,
            name: member.declarator.name.clone(),
            offset,
        }
    }

    fn gen_expr_from_obj(&self, o: &Obj) -> ir::Expr {
        let content = ir::ExprType::Ident(o.clone());
        let span = Span{start_index: 0, end_index: 0};
        ir::Expr{content, ty: o.ty.clone(), span}
    }


    // @Naming: Rename it to create_local_obj.
    fn create_obj(&mut self, ty: &Type, name: &str) -> Obj {
        let mut size: i32 = sizeof(ty);
        let aligned_offset = align_to(self.cur_offset, ty.align());
        self.cur_offset = aligned_offset;
        let obj = Obj{name: name.to_string(), ty: ty.clone(), offset: self.cur_offset, is_global: false};
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
                self.cur_scope_tracker.enter_new_scope();
                let stmts = self.analyze_items(items);
                self.cur_scope_tracker.exit_current_scope();
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
                let ty = Type::Int;
                ir::Expr {content, ty, span}
            }
            Binary(lhs, rhs, tokenKind) => {
                let mut lhs = self.analyze_expr(lhs);
                let mut rhs = self.analyze_expr(rhs);
                match tokenKind {
                    // deal with pointer arithmatic
                    Plus => {
                        if lhs.is_pointer_or_array() && rhs.is_pointer_or_array() {
                            println!();
                            print_error_at(lhs.span, "error: both lhs and rhs are of ptr type");
                            print_error_at(rhs.span, "error: both lhs and rhs are of ptr type");
                        }
                        if lhs.is_integer() && rhs.is_pointer_or_array() {
                            swap(&mut lhs, &mut rhs);
                        }
                        if lhs.is_pointer_or_array() && rhs.is_integer() {
                            let mut scal: i32;
                            match &lhs.ty {
                                Pointer_To(..) => scal = sizeof(&lhs.ty),
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
                        if lhs.is_integer() && rhs.is_pointer_or_array() {
                            print_error_at(rhs.span, "error: integer - ptr");
                        }
                        if is_pointer_or_array(&lhs.ty) && rhs.is_integer() {
                            let mut scal: i32;
                            match &lhs.ty {
                                Pointer_To(..) => scal = sizeof(&lhs.ty),
                                ArrayOf(element_type, _) => scal = sizeof(element_type),
                                _ => scal = 8,
                            }
                            rhs = scal_expr(&rhs, Mul, scal);
                        } else if is_pointer_or_array(&lhs.ty) && is_pointer_or_array(&rhs.ty) {
                            let mut basic_ty = lhs.ty.clone();
							if let ArrayOf(basic, _) = &lhs.ty {
								basic_ty = *basic.clone();
							} else if let Pointer_To(basic) = &lhs.ty {
								basic_ty = *basic.clone();
                            }
							if lhs.ty != rhs.ty {
								print_error_at(rhs.span, "pointer arithmatic warning: type doesn't match");
							}
                            let ty = Type::Int;
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
            CommaExpression(lhs, rhs) => {
                let rhs = self.analyze_expr(rhs);
                let lhs = self.analyze_expr(lhs);
                let ty = rhs.ty.clone();
                let content = ExprType::CommaExpression(Box::new(lhs), Box::new(rhs));
                ir::Expr{content, ty, span}
            },
            Assign(lhs, rhs) => {
                let rhs = self.analyze_expr(rhs);
                let lhs = self.analyze_expr(lhs);
                if !can_be_lvalue(&lhs) {
                    // @Incomplete: more precise error report: the reason why lhs cannot be lvalue
                    let err_info = format!("lhs cannot be lvalue: {:?}", &lhs.ty);
                    print_error_at(lhs.span, &err_info);
                }
                if !can_assign(&lhs.ty, &rhs.ty) {
                    let err_info = format!("mismatch types: try to assign type {:?} to type {:?}",
                    &rhs.ty, &lhs.ty);
                    print_error_at(lhs.span, &err_info);
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
                    Pointer_To(base) => {
                        *base.clone()
                    }
                    ArrayOf(base, _) => {
                        *base.clone()
                    }
                    _ => {
                        let err_msg = format!("semantic error: invalid dereferencing: try to dereference {:?}", val.ty);
                        print_error_at(expr.span, &err_msg);
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
                let obj = if let Some(o) = self.cur_scope_tracker.resolve_symbol(s) {
                    o.clone()
                } else {
                    let err_info = format!("semantic error: symbol '{}' not found", s);
                    print_error_at(expr.span, &err_info);
                    // @Watchout: create_obj() changes the offset, which it should not.
                    self.create_obj(&ty_none, &s)
                };
                let ty = obj.ty.clone();
                let content = ExprType::Ident(obj);
                ir::Expr{content, ty, span}
            }
            RequestStructMember(struct_ref, member_name) => {
                let mut struct_ref = self.analyze_expr(struct_ref);
                let mut offset: i32 = 0;
                let mut ty = ty_none;
                let mut cur_ty = struct_ref.ty.clone();
                if let Pointer_To(inner) =  cur_ty.clone() {
                    cur_ty = *inner;
                    let content = ExprType::Deref(Box::new(struct_ref));
                    struct_ref = ir::Expr{content, ty: cur_ty.clone(), span};
                }

                if let Tagged_Struct(tag_name) = cur_ty.clone() {
                    let result = self.cur_scope_tracker.resolve_tag(&tag_name).clone();
                    if let Some(st) = result {
                        cur_ty = Struct(st.clone());
                    } else {
                        let err_info = format!("it has incomplete struct or union type definition.");
                        print_error_at(struct_ref.span, &err_info);
                    }
                }

                if let Struct(st) = cur_ty {
                    match st.get_member(&member_name) {
                        Ok(m) => {
                            offset = m.offset;
                            ty = m.ty;
                        },
                        Err(err) => print_error_at(expr.span, &err),
                    }
                } else {
                    let err_info = format!("semantic error: trying to request struct member, but this is not even a struct!");
                    print_error_at(struct_ref.span, &err_info);
                }

                let content = ir::ExprType::RequestStructMember(Box::new(struct_ref), offset);
                ir::Expr{content, ty, span}
            }
            ArrayIndexing(arr_ref, indices) => {
                let mut analyzed_indices = Vec::new();
                let mut arr_ref = self.analyze_expr(arr_ref);
                for index in indices {
                    let analyzed_index = self.analyze_expr(index);
                    analyzed_indices.push(analyzed_index);
                }
                let cur_ref = &mut arr_ref;
                for index in analyzed_indices {
                    // a[b] is *(a+b); pointer addition is commutative, so b[a] is valid too.
                    let (base, idx) = if !cur_ref.is_pointer_or_array() && index.is_pointer_or_array() {
                        (index, cur_ref.clone())
                    } else {
                        (cur_ref.clone(), index)
                    };
                    // @TODO: Check whether the data type of idx is integer.
                    // type checking
                    if !base.is_pointer_or_array() {
                        print_error_at(base.span, "subscripted value is neither array nor pointer nor vector");
                    } else {
                        // Array indexing is converted to pointer arithmatic and dereferencing
                        // pointer arithmatic.
                        let element_size = match &base.ty {
                                Pointer_To(pointee) => sizeof(pointee),
                                ArrayOf(element_type, _) => sizeof(element_type),
                                _ => 8,
                            };
                        let scaled = scal_expr(&idx, Mul, element_size);
                        let pointer_arithmatic = ExprType::Binary(Box::new(base.clone()), Box::new(scaled), OP::Plus);
                        let pointer_arithmatic_expr = ir::Expr {
                            content: pointer_arithmatic,
                            ty: base.ty.clone(),
                            span,
                        };

                        // Dereferencing.
                        let base_ty = match &pointer_arithmatic_expr.ty {
                            Pointer_To(base) => {
                                *base.clone()
                            }
                            ArrayOf(base, _) => {
                                *base.clone()
                            }
                            _ => {
                                let err_msg = format!("semantic error: invalid dereferencing:
                                try to dereference {:?}", pointer_arithmatic_expr.ty);
                                print_error_at(expr.span, &err_msg);
                                base.ty.clone()
                            }
                        };

                        let deref = ExprType::Deref(Box::new(pointer_arithmatic_expr));
                        let deref_expr = ir::Expr {
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
                // that's an error, but the function name is intentionally left undeclared (currently). 
                // @Future: The above problem will be solved when we add function declaration. At that time every valid function call can
                // find its declaration without adding obj at here in the following code (because the obj is added after the compiler
                // dealt with the corresponding declaration).
                match &ident.content {
                    Ident(s) => {
                        if let Some(_) = self.cur_scope_tracker.resolve_symbol(s) {

                        } else {
                            let obj = self.create_obj(&ty_none, &s);
                            self.cur_scope_tracker.add_obj(obj.clone());
                        }
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
                let len = s.len() + 1;
                let ty: Type = ArrayOf(Box::new(Type::Char), len.try_into().unwrap());

                let global_obj = create_global_obj(&unique_name, &ty);
                // @Cleanup: We don't need to add the obj to global scope
                self.cur_scope_tracker.add_private_global_obj(global_obj.clone());
                let mut value_in_bytes = s.clone();
                value_in_bytes.push(b'\0');
                let global_decl = ir::Declaration{obj: global_obj.clone(), init_value: Some(value_in_bytes)};
                self.global_decls.push(global_decl);

                let unique_symbol = ExprType::Ident(global_obj);
                ir::Expr{content: unique_symbol, ty, span}
            }
            Paren(inner) => self.analyze_expr(inner),
            StmtExpr(items) => {
                self.cur_scope_tracker.enter_new_scope();
                let stmts = self.analyze_items(items);
                self.cur_scope_tracker.exit_current_scope();
                let ty = match stmts.last() {
                    Some(ir::StmtType::Ex(e)) => e.ty.clone(),
                    _ => {
                        print_error_at(span, "a statement expression must end with an expression statement");
                        ty_none
                    }
                };
                let content = ExprType::StmtExpr(stmts);
                ir::Expr{content, ty, span}
            },
        }
    }
}

fn pointer_to(ty: &Type) -> Type {
    let base = Box::new(ty.clone());
    Pointer_To(base)
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
    Func(return_type)
}

// evaluate whether a expression of right type can be assigned to a "stuff"
// of left type
fn is_integer(ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Char)
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
    match &expr.content {
        ExprType::FunCall(_, _) => false,
        ExprType::Ident(_) => {
            if let ArrayOf(_, _) = expr.ty {
                false
            } else {
                true
            }
        }
        ExprType::CommaExpression(_, rhs) => {
            return can_be_lvalue(rhs);
        },
		_ => true,
    }
}

fn can_be_rvalue(expr: &Expr) -> bool {
    return true;
}

pub fn is_pointer_or_array(t: &Type) -> bool {
    match t {
        Pointer_To(_) | ArrayOf(_, _) => true,
        _ => false
    }
}

pub fn is_pointer(t: &Type) -> bool {
    match t {
        Pointer_To(_) => true,
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
        ty: Type::Int,
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

fn create_global_obj(name: &str, base_type: &Type) -> Obj {
    let mut cur_type = base_type.clone();
    let mut size: i32 = sizeof(base_type);
    let obj = Obj{name: name.to_string(), ty: base_type.clone(), offset: 0, is_global: true};
    // @Smell: This line should be executed outside of this function.
    // self.cur_offset += size;
    obj
}

// @Duplication: Duplicate with error reporter in parse.rs.
fn print_error_at(span: Span, info: &str) {
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
    println!("{}", err_msg);
    exit(1);
}

pub fn align_to(n: i32, align: i32) -> i32 {
    let extra = n % align;
    let base = n - extra;
    match extra {
        0 => base,
        _ => base + align,
    }
}