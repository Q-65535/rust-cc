use std::{io::{self, Write}, collections::{VecDeque, HashMap}, process::exit, mem::swap};
use colored::*;
use crate::parse::{self, *};
use crate::lex::{self, *};
use crate::ir;
use ExprType::*;
use Struct_Or_Union::*;
use StmtType::*;
use TokenKind::{Plus, Minus, Mul, Div, PlusAssignment, MinusAssignment, MulAssignment, DivAssignment, Eq, Neq, LT, LE, GT, GE};
use BlockItem::*;
use crate::SRC;
use crate::common::{self, *};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Symbol_Attribute {
    pub is_typedef: bool,
    pub is_static:  bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Pointer_To(Box<Type>),
    Int,
    Long,
    Short,
    Char,
    Bool,
    Void,
    ArrayOf(Box<Type>, usize),
    Func{return_type: Box<Type>, param_types: Vec<Type>},
    Struct(ir::Struct),
    Union(ir::Struct),
    Enum,
    Tag(String),
    ty_none,
}
use Type::*;

impl Type {
    fn align(&self) -> usize {
        match self {
            Pointer_To(_) => 8,
            Type::Int => 4,
            Type::Long => 8,
            Type::Short => 2,
            Type::Char => 1,
            Type::Bool => 1,
            Type::Void => 1,
            ArrayOf(element_ty, len) => element_ty.align(),
            Func{..} => 8,
            Struct(st) => st.align,
            Union(st) => st.align,
            Enum => 4,
            Tag(_) => 0,
            ty_none => 1,
        }
    }
}

pub fn sizeof(ty: &Type) -> usize {
    match ty {
        Pointer_To(_) => 8,
        Type::Int => 4,
        Type::Long => 8,
        Type::Short => 2,
        Type::Char => 1,
        Type::Bool => 1,
        Type::Void => 1,
        ArrayOf(element_ty, len) => sizeof(element_ty) * len,
        Func{..} => 8,
        Struct(st) => st.size,
        Union(st) => st.size,
        Enum => 4,
        Tag(tag_name) => {
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
    pub offset: usize,
    pub is_global: bool,
    // @TODO: Add position info.
    // When a variable is already defined, the compiler should tell where the variable is defined.
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Object(Obj),
    Typedef(Type),
    Enum(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub symbols: HashMap<String, Symbol>,
    pub tags:    HashMap<String, Type>,
}

impl Scope {
    pub fn new() -> Self {
        Scope{
            symbols: HashMap::new(),
            tags:    HashMap::new(),
        }
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

    pub fn contains_symbol(&self, name: &str) -> bool {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if current_scope.symbols.contains_key(name) {
                return true;
            }
        }
        return false;
    }

    pub fn contains_symbol_at_current_scope(&self, name: &str) -> bool {
        let current_scope = &self.scopes[self.current_scope_index];
        if current_scope.symbols.contains_key(name) {
            return true;
        }
        return false;
    }

    pub fn resolve_typedef_alias(&self, name: &str) -> Option<&Type> {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if let Some(symbol) = current_scope.symbols.get(name) {
                if let Symbol::Typedef(ty) = symbol {
                    return Some(ty);
                }
            }
        }
        return None
    }

    pub fn resolve_typedef_at_current_scope(&self, name: &str) -> Option<&Type> {
        let current_scope = &self.scopes[self.current_scope_index];
        if let Some(symbol) = current_scope.symbols.get(name) {
            if let Symbol::Typedef(ty) = symbol {
                return Some(ty);
            }
        }
        return None;
    }

    pub fn add_typedef_alias(&mut self, name: &str, the_type: Type) {
        debug_assert!(self.resolve_typedef_at_current_scope(name) == None);
        let current_scope = &mut self.scopes[self.current_scope_index];
        current_scope.symbols.insert(name.to_string(), Symbol::Typedef(the_type));
    }

    pub fn resolve_object(&self, name: &str) -> Option<&Obj> {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if let Some(symbol) = current_scope.symbols.get(name) {
                if let Symbol::Object(obj) = symbol {
                    return Some(obj);
                }
            }
        }
        return None
    }

    pub fn resolve_object_at_current_scope(&self, name: &str) -> Option<&Obj> {
        let current_scope = &self.scopes[self.current_scope_index];
        if let Some(symbol) = current_scope.symbols.get(name) {
            if let Symbol::Object(obj) = symbol {
                return Some(obj);
            }
        }
        return None;
    }

    pub fn add_object(&mut self, obj: Obj) {
        let name = &obj.name;
        debug_assert!(!self.contains_symbol_at_current_scope(name));
        let current_scope = &mut self.scopes[self.current_scope_index];
        current_scope.symbols.insert(name.to_string(), Symbol::Object(obj));
    }

    pub fn resolve_enum(&self, name: &str) -> Option<u64> {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if let Some(symbol) = current_scope.symbols.get(name) {
                if let Symbol::Enum(number) = symbol {
                    return Some(*number);
                }
            }
        }
        return None
    }

    pub fn resolve_enum_at_current_scope(&self, name: &str) -> Option<u64> {
        let current_scope = &self.scopes[self.current_scope_index];
        if let Some(symbol) = current_scope.symbols.get(name) {
            if let Symbol::Enum(number) = symbol {
                return Some(*number);
            }
        }
        return None;
    }

    pub fn add_enum(&mut self, name: &str, number: u64) {
        debug_assert!(!self.contains_symbol(name));
        let current_scope = &mut self.scopes[self.current_scope_index];
        current_scope.symbols.insert(name.to_string(), Symbol::Enum(number));
    }

    pub fn resolve_tag(&self, name: &str) -> Option<&Type> {
        let index = self.current_scope_index;
        for i in (0..=index).rev() {
            let current_scope = &self.scopes[i];
            if let Some(the_type) = current_scope.tags.get(name) {
                return Some(the_type)
            }
        }
        return None
    }

    pub fn resolve_tag_at_current_scope(&self, s: &str) -> Option<&Type> {
        let current_scope = &self.scopes[self.current_scope_index];
        return current_scope.tags.get(s);
    }

    pub fn add_tag(&mut self, name: &str, the_type: &Type) {
        let current_scope = &mut self.scopes[self.current_scope_index];
        debug_assert!(current_scope.tags.get(name) == None);
        current_scope.tags.insert(name.to_string(), the_type.clone());
    }
}

pub struct ProgramAnalyzer {
    pub global_decls: Vec<ir::Declaration>,
    pub scope_manager: ScopeManager,
    pub current_local_var_offset: usize,
    pub current_function_return_type: Type,
    pub unique_string_name_index: usize,
}

impl ProgramAnalyzer {
    pub fn new() -> Self {
        let scope = Scope::new();
        ProgramAnalyzer{
                        global_decls: Vec::new(),
                        scope_manager: ScopeManager::new(),
                        current_local_var_offset: 0,
                        current_function_return_type: Type::ty_none,
                        unique_string_name_index: 0,
                        }
    }

    pub fn analyze(mut self, mut program: Program) -> ir::AnalyzedProgram {
        use ir::Function;
        let mut afuns: Vec<ir::Function> = Vec::new();
        // Record all symbols at first pass.
        for unit in &mut program.translation_units {
            match unit {
                parse::TranslationUnit::FunctionDef(fun) => {
                    let (mut base_type, mut symbol_attribute) = self.analyze_decl_spec(&fun.return_type_specifier);
                    let (function_type, name) = self.resolve_declarator(&base_type, &fun.declarator);
                    // It is not allowed that function, variable or typedef name have the same name in the same scope.
                    // So we only check whether we encounter a duplicate name without considering it is function, variable or typedef name.
                    if self.scope_manager.contains_symbol_at_current_scope(&name) {
                        let err_info = format!("semantic error: {} redeclared as a symbol", name);
                        print_error_at(fun.declarator.span, &err_info);
                        exit(1);
                    }
                    let o = create_global_obj(&name, &function_type);
                    self.scope_manager.add_object(o);
                }
                parse::TranslationUnit::GlobalDecl(decl) => {
                    let mut batch_global_decls = self.analyze_global_decl(decl);
                    self.global_decls.append(&mut batch_global_decls);
                }
            }
        }
        // Analyze function bodies at second pass.
        for unit in &mut program.translation_units {
            if let parse::TranslationUnit::FunctionDef(fun) = unit {
                let afun = self.analyze_function(fun);
                afuns.push(afun);
            }
        }
        ir::AnalyzedProgram{afuns, global_decls: self.global_decls}
    }

    pub fn analyze_typedef(&mut self, base_type: &Type, declarator: &Declarator) {
        let (final_type, name) = self.resolve_declarator(base_type, declarator);
        self.scope_manager.add_typedef_alias(&name, final_type);
    }

    pub fn analyze_global_decl(&mut self, decl: &mut Declaration) -> Vec::<ir::Declaration> {
        let mut decls: Vec<ir::Declaration> = Vec::new();
        let (base_type, symbol_attribute) = self.analyze_decl_spec(&decl.decl_spec);
        if symbol_attribute.is_typedef {
            for declarator in &mut decl.declarators {
                self.analyze_typedef(&base_type, declarator);
            }
            return decls;
        }
        for declarator in &mut decl.declarators {
            let (final_type, name) = self.resolve_declarator(&base_type, declarator);

            if self.scope_manager.contains_symbol_at_current_scope(&name) {
                let err_info = format!("semantic error: {} redeclared as a symbol", name);
                print_error_at(declarator.span, &err_info);
                exit(1);
            }
            let mut init_value = None;
            if let Some(expr) = &mut declarator.init_expr {
                let analyzed_expr = self.analyze_expr(expr);
                // @Future: Currently, we only support constant number assignment.
                // We will add array and struct initialization expr assignment in the future.
                match analyzed_expr.content {
                    ir::ExprType::Natural_Number(n) => {
                        if can_assign(&final_type, &analyzed_expr.ty) {
                            let value_in_bytes = n.to_le_bytes();
                            init_value = Some(value_in_bytes.to_vec());
                        } else {
                            let err_info = format!("mismatch types: {} type is {:?}, but expression type is {:?}",
                            name, &final_type, &analyzed_expr.ty);
                            print_error_at(declarator.span, &err_info);
                            exit(1);
                        }
                    }
                    _ => {
                        let err_info = format!("This is not a constant number expression!");
                        print_error_at(analyzed_expr.span, &err_info);
                        exit(1);
                    }
                }
            }
            let object = create_global_obj(&name, &final_type);
            self.scope_manager.add_object(object.clone());
            // A function declarator with no body (e.g. `int printf();`) is a
            // prototype, not a variable definition. Register it in scope so
            // calls resolve, but do NOT emit a data object for it — doing so
            // would define a bogus symbol that overrides the real function.
            if let Type::Func{..} = final_type {
                continue;
            }
            let analyzed_decl = ir::Declaration{obj: object.clone(), init_value};
            decls.push(analyzed_decl);
        }
        decls
    }

    fn resolve_type_with_suffix(&mut self, base_type: &Type, suffix: &DeclaratorSuffix) -> Type {
        match suffix {
            DeclaratorSuffix::ArrayLen(len, inner_suffix) => {
                if let Some(inner_suffix) = inner_suffix {
                    let cur_type = self.resolve_type_with_suffix(base_type, inner_suffix);
                    return array_of(&cur_type, *len);
                } else {
                    return array_of(base_type, *len);
                }
            },
            DeclaratorSuffix::FunParam(params) => {
                let return_type = base_type.clone();
                let mut param_types = Vec::new();
                for param in params {
                    let (param_base_type, symbol_attribute) = self.analyze_decl_spec(&param.decl_spec);
                    let (param_final_type, _) = self.resolve_declarator(&param_base_type, &param.declarator);
                    param_types.push(param_final_type);
                }
                let return_type = Box::new(return_type);
                return Type::Func{return_type, param_types};
            },
        }
    }

    fn resolve_abstract_declarator(&mut self, base_type: &Type, declarator: &Abstract_Declarator) -> Type {
        // deal with pointers
        let mut cur_type = base_type.clone();
        for i in 0..declarator.star_count {
            cur_type = pointer_to(&cur_type);
        }
        // deal with suffix
        if let Some(suffix) = &declarator.suffix {
            cur_type = self.resolve_type_with_suffix(&cur_type, suffix);
        }
        if let Some(inner_declarator) = &declarator.direct_abstract_declarator {
            cur_type = self.resolve_abstract_declarator(&cur_type, &*inner_declarator);
        }

        // If the final type is a tag, We want to make sure that
        // it can resove to a concrete struct, and do the Resolvation.
        if let Tag(tag_name) = cur_type.clone() {
            match self.scope_manager.resolve_tag(&tag_name) {
                Some(the_type) => cur_type = the_type.clone(),
                None => {
                    let err_info = format!("storage size of {} is unkonwn", &tag_name);
                    print_error_at(declarator.span, &err_info);
                    exit(1);
                },
            }
        }
        return cur_type;
    }

    fn resolve_declarator(&mut self, base_type: &Type, declarator: &Declarator) -> (Type, String) {
        // deal with pointers
        let mut cur_type = base_type.clone();
        let mut name = "empty_declarator_name".to_string();
        for i in 0..declarator.star_count {
            cur_type = pointer_to(&cur_type);
        }
        // deal with suffix
        if let Some(suffix) = &declarator.suffix {
            cur_type = self.resolve_type_with_suffix(&cur_type, suffix);
        }
        match &*declarator.direct_declarator {
            Direct_Declarator::Identifier(ident) => {
                name = ident.clone();
            },
            Direct_Declarator::Paren_Enclosed_Declarator(inner_declarator) => {
                (cur_type, name) = self.resolve_declarator(&cur_type, &inner_declarator);
            },
        }
        // If the final type is a tag, We want to make sure that
        // it can resove to a concrete struct, and do the Resolvation.
        if let Tag(tag_name) = cur_type.clone() {
            match self.scope_manager.resolve_tag(&tag_name) {
                Some(the_type) => cur_type = the_type.clone(),
                None => {
                    let err_info = format!("storage size of {} is unkonwn", &tag_name);
                    print_error_at(declarator.span, &err_info);
                    exit(1);
                },
            }
        }

        // @TODO: Remove this because "typdef void x;" is valid c code,
        // but analyze_typedef calls current function.
        if cur_type == Void {
            let err_info = format!("variable declared void!");
            print_error_at(declarator.span, &err_info);
            exit(1);
        }
        return (cur_type, name);
    }

    pub fn analyze_function(&mut self, fun: &mut Function) -> ir::Function {
        // current_local_var_offset will be used to determine the position of all local variables in current
        // to-be-analyzed function. Thus, at the start of each function analyzation, we must reset it.
        self.current_local_var_offset = 0;

        let (base_type, symbol_attribute) = self.analyze_decl_spec(&fun.return_type_specifier);
        let is_static = symbol_attribute.is_static;
        let (final_type, name) = self.resolve_declarator(&base_type, &fun.declarator);
        if let Func{return_type, ..} = final_type {
            self.current_function_return_type = *return_type;
        } else {
            let err_info = format!("compiler bug: we are analyzing a function definition,
            but the data type resolved is not function!.");
            print_error_at(fun.declarator.span, &err_info);
            exit(1);
        }
        // We must enter scope before analyzing function
        // parameters since function parameters are also in
        // the function body scope.
        self.scope_manager.enter_new_scope();
        let mut analyzed_params: Vec<Obj> = Vec::new();
        if let Some(DeclaratorSuffix::FunParam(params)) = &fun.declarator.suffix {
            for param in params {
                let p = self.analyze_param(param);
                analyzed_params.push(p);
            }
        } else {
            let err_info = format!("compiler bug: the function doesn't have parameter filed.");
            print_error_at(fun.declarator.span, &err_info);
            exit(1);
        }
        let mut stmts = self.analyze_block(&mut fun.items);
        let stack_size = self.current_local_var_offset;
        self.scope_manager.exit_current_scope();
        ir::Function{name, params: analyzed_params, stmts, stack_size, is_static}
    }

    fn analyze_block(&mut self, items: &mut Vec<BlockItem>) -> Vec<ir::StmtType> {
        let mut stmts: Vec<ir::StmtType> = Vec::new();
        for item in items {
            match item {
                Stmt(stmt) => stmts.push(self.analyze_stmt(stmt)),
                Decl(decl) => stmts.append(&mut self.analyze_decl(decl)),
            }
        }
        stmts
    }

    fn analyze_block_in_new_scope(&mut self, items: &mut Vec<BlockItem>) -> Vec<ir::StmtType> {
        self.scope_manager.enter_new_scope();
        let stmts = self.analyze_block(items);
        self.scope_manager.exit_current_scope();
        stmts
    }

    fn analyze_param(&mut self, param: &Parameter) -> Obj {
        let (base_type, symbol_attribute) = self.analyze_decl_spec(&param.decl_spec);
        let (final_type, name) = self.resolve_declarator(&base_type, &param.declarator);
        if self.scope_manager.contains_symbol_at_current_scope(&name) {
            let err_info = format!("fatal error: parameter variable {} already defined", &name);
            print_error_at(param.declarator.span, &err_info);
            exit(1);
        } else {
            let obj = self.create_local_obj(&final_type, &name);
            self.scope_manager.add_object(obj.clone());
            return obj;
        }
    }


    // After analyzation, declarations are all resolved to creating obj and assignment statement.
    fn analyze_decl(&mut self, decl: &mut Declaration) -> Vec<ir::StmtType> {
        let mut stmts: Vec<ir::StmtType> = Vec::new();
        let (base_type, symbol_attribute) = self.analyze_decl_spec(&decl.decl_spec);
        if symbol_attribute.is_typedef {
            for declarator in &mut decl.declarators {
                self.analyze_typedef(&base_type, declarator);
            }
            return stmts;
        }
        for declarator in &mut decl.declarators {
            let (final_type, name) = self.resolve_declarator(&base_type, declarator);
            if self.scope_manager.contains_symbol_at_current_scope(&name) {
                let err_info = format!("variable {} already defined", name);
                print_error_at(declarator.span, &err_info);
                exit(1);
            }
            // @Fix: If it is a function declaration, we shouldn't allocate
            // stack space to it? But currently create_local_obj() will definitely
            // allocate space accroding to the size of the given type.
            let obj = self.create_local_obj(&final_type, &name);
            self.scope_manager.add_object(obj.clone());
            if let Some(expr) = &mut declarator.init_expr {
                let lhs = self.gen_expr_from_obj(&obj);
                let rhs = self.analyze_expr(expr);
                let assignment_expr = gen_assign_expr(lhs, rhs);
                let expr_stmt = ir::StmtType::Ex(assignment_expr);
                stmts.push(expr_stmt);
            }
        }
        stmts
    }

    fn analyze_decl_spec(&mut self, decl_specs: &Vec<Decl_Spec>) -> (Type, Symbol_Attribute) {
        const VOID:  u32 = 1 << 0;
        const BOOL:  u32 = 1 << 2;
        const CHAR:  u32 = 1 << 4;
        const SHORT: u32 = 1 << 6;
        const INT:   u32 = 1 << 8;
        const LONG:  u32 = 1 << 10;
        const OTHER: u32 = 1 << 12;

        let mut var_attribute = Symbol_Attribute::default();
        let mut count: u32 = 0;
        let mut cur_type = Type::Int;
        for spec in decl_specs {
            match spec {
                Decl_Spec::Typedef => {
                    var_attribute.is_typedef = true;
                    continue;
                },
                Decl_Spec::Static => {
                    var_attribute.is_static = true;
                    continue;
                },
                Decl_Spec::Typedef_Name(name) => {
                    let result = self.scope_manager.resolve_typedef_alias(name);
                    if let Some(ty) = result {
                        cur_type = ty.clone();
                    } else {
                        println!("unknown typedef name :{}", name);
                        exit(1);
                    }
                    count += OTHER;
                    continue;
                },
                Decl_Spec::Int => {
                    count += INT;
                },
                Decl_Spec::Long => {
                    count += LONG;
                },
                Decl_Spec::Short => {
                    count += SHORT;
                },
                Decl_Spec::Char => {
                    count += CHAR;
                },
                Decl_Spec::Bool => {
                    count += BOOL;
                },
                Decl_Spec::Void => {
                    count += VOID;
                },
                Decl_Spec::Struct_Union(st) => {
                    cur_type = self.analyze_struct_union(st);
                    count += OTHER;
                    continue;
                },
                Decl_Spec::Enum(enum_spec) => {
                    cur_type = self.analyze_enum(enum_spec);
                    count += OTHER;
                    continue;
                },
            }

            cur_type = match count {
                _ if count == VOID              => Type::Void,
                _ if count == BOOL              => Type::Bool,
                _ if count == CHAR              => Type::Char,
                _ if count == SHORT             => Type::Short,
                _ if count == SHORT + INT       => Type::Short,
                _ if count == INT               => Type::Int,
                _ if count == LONG              => Type::Long,
                _ if count == LONG + INT        => Type::Long,
                _ if count == LONG + LONG       => Type::Long,
                _ if count == LONG + LONG + INT => Type::Long,
                _ => {
                    println!("invalid type");
                    exit(1);
                }
            };
        }
        if var_attribute.is_static && var_attribute.is_typedef {
            println!("typedef and static may not be used together");
            exit(1);
        }
        return (cur_type, var_attribute);
    }

    // @Refactor: Refactor this to the process like analyze_enum().
    fn analyze_struct_union(&mut self, st: &Struct_Union_Specifier) -> Type {
        let mut analyzed_members = Vec::new();
        let mut offset: usize = 0;
        let mut struct_align: usize = 1;
        let mut max_member_size: usize = 0;
        let mut the_type;
        if let Some(members) = &st.members {
            for m in members {
                let mut am = self.analyze_struct_member(m, offset);
                let member_align = am.ty.align();
                if (st.kind == Is_Struct) {
                    offset = align_to(offset, member_align);
                    am.offset = offset;
                    offset += sizeof(&am.ty);
                }
                if struct_align < member_align {
                    struct_align = member_align;
                }
                if max_member_size < sizeof(&am.ty) {
                    max_member_size = sizeof(&am.ty);
                }
                analyzed_members.push(am);
            }
            let struct_size = match st.kind {
                Is_Struct => align_to(offset, struct_align),
                Is_Union => align_to(max_member_size, struct_align),
            };
            let the_struct = ir::Struct {
                members: analyzed_members,
                size: struct_size,
                align: struct_align,
            };
            the_type = match st.kind {
                Is_Struct => Struct(the_struct),
                Is_Union => Union(the_struct),
            };
        } else {
            if let Some(name) = &st.name {
                the_type = if let Some(the_type) = self.scope_manager.resolve_tag(name) {
                    the_type.clone()
                } else {
                    Tag(name.clone())
                };
            } else {
                println!("fatal compiler bug: the strcut_union specifier have nither name nor member list!");
                exit(1);
            }
        }

        // Deal with scope stuff.
        if let Some(name) = &st.name {
            if let Some(members) = &st.members {
                if self.scope_manager.resolve_tag_at_current_scope(name).is_some() {
                    let err_info = format!("semantic error: redefinition of tag name: '{}'", name);
                    // @TODO add span info to declaration specifier
                    print_error_at(Span{start_index: 0, end_index: 0}, &err_info);
                    exit(1);
                } else {
                    self.scope_manager.add_tag(name, &the_type);
                }
            }
        }

        return the_type;
    }

    fn analyze_enum(&mut self, enum_spec: &Enum_Specifier) -> Type {
        if let Some(name) = &enum_spec.name {
            if let Some(the_type) = self.scope_manager.resolve_tag_at_current_scope(name) {
                // If it has a tag name, and the tag name is already registerd at
                // current scope, enumerator list shall not appear, otherwise it is an semantic error.
                if enum_spec.enumerators.is_some() {
                    let err_info = format!("semantic error: redefinition of tag name: '{}'", name);
                    print_error_at(Span{start_index: 0, end_index: 0}, &err_info);
                    exit(1);
                } else {
                    return the_type.clone();
                }
            } else {
                let the_type = Type::Enum;
                if let Some(enumerators) = &enum_spec.enumerators {
                    self.scope_manager.add_tag(name, &the_type);
                    let mut value: u64 = 0;
                    for e in enumerators {
                        if let Some(expr) = &e.constant_expr {
                            // @Incomplete: For now, we just consider single constant number,
                            // not actual constant expression which may involve operators.
                            if let Natural_Number(n) = &expr.content {
                                value = *n;
                            }
                        }
                        self.scope_manager.add_enum(&e.name, value);
                        value += 1;
                    }
                    return the_type;
                } else {
                    let err_info = format!("semantic error: undefined tag name: '{}'", name);
                    print_error_at(Span{start_index: 0, end_index: 0}, &err_info);
                    exit(1);
                }
            }
        } else {
            if let Some(enumerators) = &enum_spec.enumerators {
                let mut value: u64 = 0;
                for e in enumerators {
                    if let Some(expr) = &e.constant_expr {
                        // @Incomplete: For now, we just consider single constant number,
                        // not actual constant expression which may involve operators.
                        if let Natural_Number(n) = &expr.content {
                            value = *n;
                        }
                    }
                    self.scope_manager.add_enum(&e.name, value);
                    value += 1;
                }
                let the_type = Type::Enum;
                return the_type;
            } else {
                println!("compiler bug: both tag name and enumerator list are None.");
                exit(1);
            }
        }
    }

    fn analyze_struct_member(&mut self, member: &Member, offset: usize) -> ir::Member {
        let (base_type, symbol_attribute) = self.analyze_decl_spec(&member.decl_spec);
        let (final_type, name) = self.resolve_declarator(&base_type, &member.declarator);
        ir::Member{
            ty: final_type,
            name: name.clone(),
            offset,
        }
    }

    fn gen_expr_from_obj(&self, o: &Obj) -> ir::Expr {
        let content = ir::ExprType::Object(o.clone());
        let span = Span{start_index: 0, end_index: 0};
        ir::Expr{content, ty: o.ty.clone(), span}
    }


    fn create_local_obj(&mut self, ty: &Type, name: &str) -> Obj {
        debug_assert!(!self.scope_manager.contains_symbol_at_current_scope(name));

        let mut size: usize = sizeof(ty);
        let aligned_offset = align_to(self.current_local_var_offset, ty.align());
        self.current_local_var_offset = aligned_offset;
        let obj = Obj{name: name.to_string(), ty: ty.clone(), offset: self.current_local_var_offset, is_global: false};
        self.current_local_var_offset += size;
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
                let casted_expr = cast(expr, &self.current_function_return_type);
                StmtType::Return(casted_expr)
            },
            Block(items) => {
                let stmts = self.analyze_block_in_new_scope(items);
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
                self.scope_manager.enter_new_scope();
                let mut init_stmts = Vec::new();
                if let Some(init) = init {
                    match init.as_mut() {
                        Decl(decl) => {
                            let mut stmts_from_decl = self.analyze_decl(decl);
                            init_stmts.append(&mut stmts_from_decl);
                        }
                        Stmt(expr_stmt) => {
                            let analyzed_expr_stmt = self.analyze_stmt(expr_stmt);
                            init_stmts.push(analyzed_expr_stmt);
                        }
                    }
                }
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
                self.scope_manager.exit_current_scope();
                StmtType::For{init: init_stmts, cond, inc, then}
            }
        }
    }

    fn analyze_expr(&mut self, expr: &mut Expr) -> ir::Expr {
        use ir::ExprType;
        use ir::OP;
        let span = expr.span;
        match &mut expr.content {
            Natural_Number(n) => gen_num_expr(*n, span),
            Binary(lhs, rhs, tokenKind) => {
                let lhs = self.analyze_expr(lhs);
                let rhs = self.analyze_expr(rhs);
                match tokenKind {
                    PlusAssignment => self.to_assign(lhs, rhs, OP::Plus),
                    MinusAssignment => self.to_assign(lhs, rhs, OP::Minus),
                    MulAssignment => self.to_assign(lhs, rhs, OP::Mul),
                    DivAssignment => self.to_assign(lhs, rhs, OP::Div),
                    _ => {
                        let op = tokenkind_to_op(tokenKind);
                        gen_binary_expr(lhs, rhs, op)
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
                let mut rhs = self.analyze_expr(rhs);
                let lhs = self.analyze_expr(lhs);
                let result = gen_assign_expr(lhs, rhs);
                return result;
            }
            PreIncrement(operand) => {
                let lhs = self.analyze_expr(operand);
                let rhs = gen_num_expr(1, span);
                return self.to_assign(lhs, rhs, OP::Plus);
            }
            // Convert A++ to `(typeof A)((A += 1) - 1)`
            PostIncrement(operand) => {
                let lhs = self.analyze_expr(operand);
                let operand_type = lhs.ty.clone();
                let rhs = gen_num_expr(1, span);
                let lhs = self.to_assign(lhs, rhs, OP::Plus);  // lhs = (A += 1)

                let rhs = gen_num_expr(1, span);
                let expr = gen_binary_expr(lhs, rhs, OP::Minus); // expr = ((A + 1) - 1)
                cast(expr, &operand_type)
            }
            PreDecrement(operand) => {
                let lhs = self.analyze_expr(operand);
                let rhs = gen_num_expr(1, span);
                return self.to_assign(lhs, rhs, OP::Minus);
            }
            // Convert A-- to `(typeof A)((A -= 1) + 1)`
            PostDecrement(operand) => {
                let lhs = self.analyze_expr(operand);
                let operand_type = lhs.ty.clone();
                let rhs = gen_num_expr(1, span);
                let lhs = self.to_assign(lhs, rhs, OP::Minus);  // lhs = (A -= 1)

                let rhs = gen_num_expr(1, span);
                let expr = gen_binary_expr(lhs, rhs, OP::Plus); // expr = ((A -= 1) + 1)
                cast(expr, &operand_type)
            }
            Not(val) => {
                todo!();
            }
            BitNot(val) => {
                todo!();
            }
            Neg(val) => {
                let val = self.analyze_expr(val);
                let common_type = get_common_type(&Type::Int, &val.ty);
                let ty = val.ty.clone();
                let content = ExprType::Neg(Box::new(val));
                let neg_expr = ir::Expr{content, ty, span};
                return cast(neg_expr, &common_type);
            }
            Deref(val) => {
                let val = self.analyze_expr(val);
                return gen_deref_expr(val);
            }
            AddrOf(val) => {
                let val = self.analyze_expr(val);
                gen_addr_of_expr(val)
            }
            Ident(s) => {
                if let Some(o) = self.scope_manager.resolve_object(s) {
                    return self.gen_expr_from_obj(o);
                } else if let Some(number) = self.scope_manager.resolve_enum(s) {
                    return gen_num_expr(number, span);
                } else {
                    let err_info = format!("semantic error: symbol '{}' doesn't exist or is nither a variable nor enum constant.", s);
                    print_error_at(expr.span, &err_info);
                    exit(1);
                };
            }
            RequestStructMember(struct_expr, member_name) => {
                let mut struct_expr = self.analyze_expr(struct_expr);
                // Automatic dereference a->b to *(a).b
                if matches!(struct_expr.ty, Pointer_To(_)) {
                    struct_expr = gen_deref_expr(struct_expr);
                }

                let mut cur_ty = struct_expr.ty.clone();
                if let Tag(tag_name) = &cur_ty {
                    let result = self.scope_manager.resolve_tag(&tag_name).clone();
                    if let Some(the_type) = result {
                        cur_ty = the_type.clone();
                    } else {
                        let err_info = format!("it has incomplete struct or union type definition.");
                        print_error_at(struct_expr.span, &err_info);
                        exit(1);
                    }
                }

                let mut offset: i32 = 0;
                let mut ty = ty_none;
                if let Union(st) | Struct(st) = &cur_ty {
                    match st.get_member(&member_name) {
                        Ok(m) => {
                            let content = ir::ExprType::RequestStructMember(Box::new(struct_expr), m.offset);
                            ir::Expr{content, ty: m.ty, span}

                        },
                        Err(err) => {
                            print_error_at(expr.span, &err);
                            exit(1);
                        }
                    }
                } else {
                    let err_info = format!("semantic error: trying to request struct member, but this is not even a struct!");
                    print_error_at(struct_expr.span, &err_info);
                    exit(1);
                }
            }
            ArrayIndexing(base_position, index) => {
                let mut base_position = self.analyze_expr(base_position);
                let mut index = self.analyze_expr(index);
                if !base_position.is_pointer_or_array() {
                    swap(&mut base_position, &mut index);
                }
                // type checking
                if !base_position.is_pointer_or_array() {
                    print_error_at(base_position.span, "subscripted value is neither array nor pointer nor vector");
                    exit(1);
                }
                if !index.is_integer() {
                    print_error_at(index.span, "array subscript is not an integer");
                    exit(1);
                }
                let element_type = match &base_position.ty {
                    Pointer_To(pointee_type) => pointee_type.clone(),
                    ArrayOf(element_type, _) => element_type.clone(),
                    _ => {
                        println!("array indexing error: the base type is nither pointer nor array");
                        exit(1);
                    },
                };
                let element_size = sizeof(&element_type);
                let scaled = scale_expr(index, element_size, OP::Mul);
                let pointer_arithmatic_expr = gen_promoted_binary_expr(base_position, scaled, OP::Plus);
                return gen_deref_expr(pointer_arithmatic_expr);
            },
            FunCall(ident, args) => {
                match &ident.content {
                    Ident(name) => {
                        // @Incomplete: GCC lets you to call a undeclared function,
                        // linker reports the error if function name doesn't exist.
                        if let Some(obj) = self.scope_manager.resolve_object(name) {
                            let obj_ty = obj.ty.clone();
                            if let Func{return_type, param_types} = obj_ty {
                                let ty = *return_type;
                                let ident = self.analyze_expr(ident);
                                let mut casted_analyzed_args = Vec::new();

                                // @Future: Add these judgements.
                                // if args.len() > param_types.len() {
                                //     print_error_at(span, "Too many arguments to call this function.");
                                //     exit(1);
                                // }
                                if args.len() < param_types.len() {
                                    print_error_at(span, "Too few arguments to call this function.");
                                    exit(1);
                                }
                                for arg_index in 0..args.len() {
                                    let arg = &mut args[arg_index];
                                    let mut analyzed_arg = self.analyze_expr(arg);
                                    if arg_index < param_types.len() {
                                        let param_type = &param_types[arg_index];
                                        if matches!(param_type, Type::Struct(..) | Type::Union(..) | Type::Tag(..)) {
                                            print_error_at(span, "passing struct or union is not supported yet");
                                            exit(1);
                                        }
                                        analyzed_arg = cast(analyzed_arg, param_type);
                                        casted_analyzed_args.push(analyzed_arg);
                                    } else {
                                        // @Temporary: For now, we just accept the "too many arguments" case.
                                        casted_analyzed_args.push(analyzed_arg);
                                    }
                                }

                                let content = ExprType::FunCall(Box::new(ident), casted_analyzed_args);
                                ir::Expr {content, ty, span}
                            }
                            else {
                                let error_message = format!("You are trying to call it as a function, but its data type is {:?}", &obj_ty);
                                print_error_at(ident.span, &error_message);
                                exit(1);
                            }
                        } else {
                            if self.scope_manager.contains_symbol(name) {
                                print_error_at(ident.span, "This symbol is not a function name!");
                            } else {
                                print_error_at(ident.span, "This is an unknown symbol");
                            }
                            exit(1);
                        }
                    }
                    _ => {
                        print_error_at(ident.span, "currently only support function name as call reference");
                        exit(1);
                    }
                }
            }
            // @Temp: We only consider compile time sizeof for now
            Sizeof_Expr(expr_content) => {
                let content = self.analyze_expr(expr_content);
                let size = sizeof(&content.ty);
                // @Future: The data type of sizeof expression is u64.
                let ty = Type::Int;
                let content = ir::ExprType::Natural_Number(size.try_into().unwrap());
                ir::Expr {content, ty, span}
            }
            Sizeof_Type_Name(type_name) => {
                let the_type = self.resolve_type_name(type_name);
                let size = sizeof(&the_type);
                let ty = Type::Int;
                let content = ir::ExprType::Natural_Number(size.try_into().unwrap());
                ir::Expr {content, ty, span}
            }
            Cast(to_be_casted_expr, type_name) => {
                let mut analyzed_expr = self.analyze_expr(to_be_casted_expr);
                let to_type = self.resolve_type_name(type_name);
                let casted_expr = cast(analyzed_expr, &to_type);
                return casted_expr;
            }
            Str(s) => {
                // We use a unique identifier as a reference to replace the original string literal.
                // The string literal shall be initialized in .data section.
                let unique_name = format!(".LC{}", self.unique_string_name_index);
                self.unique_string_name_index += 1;
                // In C, a string ends with an extra \0 character, so the array length +1.
                let len = s.len() + 1;
                let ty: Type = ArrayOf(Box::new(Type::Char), len);

                let global_obj = create_global_obj(&unique_name, &ty);
                // Although the content of this obj is stored in .data section, it can only be accessed
                // at current scope. So we add the obj in current scope.
                self.scope_manager.add_object(global_obj.clone());
                let mut value_in_bytes = s.clone();
                value_in_bytes.push(b'\0');
                let global_decl = ir::Declaration{obj: global_obj.clone(), init_value: Some(value_in_bytes)};
                self.global_decls.push(global_decl);

                let unique_symbol = ExprType::Object(global_obj);
                ir::Expr{content: unique_symbol, ty, span}
            }
            Paren(inner) => self.analyze_expr(inner),
            StmtExpr(items) => {
                let stmts = self.analyze_block_in_new_scope(items);
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

    // Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
    // where tmp is a fresh pointer variable.
    fn to_assign(&mut self, lhs: ir::Expr, rhs: ir::Expr, op: ir::OP) -> ir::Expr {
        let span = Span::merge(lhs.span, rhs.span);
        let lhs_addr_expr = gen_addr_of_expr(lhs);
        let temp_obj = self.create_local_obj(&lhs_addr_expr.ty, "");
        let temp_obj_expr = self.gen_expr_from_obj(&temp_obj);
        let expr_1 = gen_assign_expr(temp_obj_expr.clone(), lhs_addr_expr); // tmp = &A

        let deref_temp_expr = gen_deref_expr(temp_obj_expr);
        let op_expr = gen_binary_expr(deref_temp_expr.clone(), rhs, op); // *tmp op B
        let expr_2 = gen_assign_expr(deref_temp_expr, op_expr); // *tmp = *tmp op B
        let ty = expr_2.ty.clone();

        let content = ir::ExprType::CommaExpression(Box::new(expr_1), Box::new(expr_2));
        ir::Expr{content, ty, span}
    }

    fn resolve_type_name(&mut self, type_name: &Type_Name) -> Type {
        let (base_type, _) = self.analyze_decl_spec(&type_name.decl_specs);
        let mut final_type = base_type;
        if let Some(abstract_declarator) = &type_name.declarator {
            final_type = self.resolve_abstract_declarator(&final_type, &abstract_declarator);
        }
        return final_type;
    }
}

fn cast(expr: ir::Expr, to_type: &Type) -> ir::Expr {
    let from_type = expr.ty.clone();
    let span = expr.span;
    let content = ir::ExprType::Cast(Box::new(expr), to_type.clone());
    let expr = ir::Expr {content, ty: to_type.clone(), span};
    if *to_type == Void {
        return expr;
    }
    if matches!(to_type, ArrayOf(..)) {
        print_error_at(span, "the cast-to type must not be array type!");
    }
    if !is_scalar_type(&from_type) || !is_scalar_type(&to_type) {
        println!("Oops! If cast-to type is not void, both cast-from and cast-to type must be scalar
        when doing type casting! Don't blame me, ChatGPT told me that.");
        println!("from_type is: {:?}, to_type is: {:?}", from_type, to_type);
        exit(1);
    } else {
        return expr;
    }
}


fn is_scalar_type(ty: &Type) -> bool {
    matches!(ty, 
        Enum | Bool | Char | Short | Int | Long | Pointer_To(..) | ArrayOf(..)
    )
}

fn pointer_to(ty: &Type) -> Type {
    let base = Box::new(ty.clone());
    Pointer_To(base)
}

fn array_of(ty: &Type, len: usize) -> Type {
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

// evaluate whether a expression of right type can be assigned to a "stuff"
// of left type
pub fn is_integer(ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Long | Type::Short | Type::Char | Type::Bool | Type::Enum)
}

fn can_assign(left: &Type, mut right: &Type) -> bool {
    // If the right is a function call, we only consdier its return type.
    if let Func {return_type, ..} = right {
        right = return_type;
    }
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
        ExprType::Object(_) => {
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

fn gen_assign_expr(lhs: ir::Expr, mut rhs: ir::Expr) -> ir::Expr {
        if !can_be_lvalue(&lhs) {
            let err_info = format!("this expr (type: {:?}) cannot be lvalue!", &lhs.ty);
            print_error_at(lhs.span, &err_info);
        }
        if !can_assign(&lhs.ty, &rhs.ty) {
            let err_info = format!("mismatch types: try to assign type {:?} to type {:?}",
            &rhs.ty, &lhs.ty);
            print_error_at(lhs.span, &err_info);
        }
        // Cast rhs to match lhs when they are not struct type.
        if !matches!(lhs.ty, Struct(..) | Union(..) | Tag(..) ) {
            rhs = cast(rhs, &lhs.ty);
        }
        let ty = lhs.ty.clone();
        let span = Span::merge(lhs.span, rhs.span);
        let content = ir::ExprType::Assign(Box::new(lhs), Box::new(rhs));
        ir::Expr{content, ty, span}
}

fn gen_num_expr(number: u64, span: Span) -> ir::Expr {
        let content = ir::ExprType::Natural_Number(number);
        let ty = if number > i32::MAX as u64 {
            Type::Long
        } else {
            Type::Int
        };
        ir::Expr {content, ty, span}
}

fn scale_expr(expr: ir::Expr, factor: usize, op: ir::OP) -> ir::Expr {
    // expr for scale num
    let span = expr.span;
    let factor_expr_content = ir::ExprType::Natural_Number(factor.try_into().unwrap());
    let factor_expr = ir::Expr {
        content: factor_expr_content,
        ty: Type::Long,
        span: expr.span,
    };
    return gen_promoted_binary_expr(expr, factor_expr, op);
}

fn gen_binary_expr(mut lhs: ir::Expr, mut rhs: ir::Expr, op: ir::OP) -> ir::Expr {
    use ir::OP;
    match op {
        OP::Plus => {
            if lhs.is_pointer_or_array() && rhs.is_pointer_or_array() {
                println!();
                print_error_at(lhs.span, "error: both lhs and rhs are of ptr type");
                print_error_at(rhs.span, "error: both lhs and rhs are of ptr type");
            }
            if lhs.is_integer() && rhs.is_pointer_or_array() {
                swap(&mut lhs, &mut rhs);
            }
            if lhs.is_pointer_or_array() && rhs.is_integer() {
                let scale = match &lhs.ty {
                    Pointer_To(pointee_type) => sizeof(pointee_type),
                    ArrayOf(element_type, _) => sizeof(element_type),
                    _ => exit(1),
                };
                rhs = scale_expr(rhs, scale, ir::OP::Mul);
            }
            gen_promoted_binary_expr(lhs, rhs, ir::OP::Plus)
        }
        OP::Minus => {
            if lhs.is_integer() && rhs.is_pointer_or_array() {
                print_error_at(rhs.span, "error: integer - ptr");
            }
            if is_pointer_or_array(&lhs.ty) && rhs.is_integer() {
                let scale = match &lhs.ty {
                    Pointer_To(pointee_type) => sizeof(pointee_type),
                    ArrayOf(element_type, _) => sizeof(element_type),
                    _ => exit(1),
                };
                rhs = scale_expr(rhs, scale, ir::OP::Mul);
                gen_promoted_binary_expr(lhs, rhs, ir::OP::Minus)
            } else if is_pointer_or_array(&lhs.ty) && is_pointer_or_array(&rhs.ty) {
                let basic_ty = match &lhs.ty {
                    ArrayOf(basic, _) => *basic.clone(),
                    Pointer_To(basic) => *basic.clone(),
                    _ => exit(1),
                };
                if lhs.ty != rhs.ty {
                    print_error_at(rhs.span, "pointer arithmatic warning: type doesn't match");
                }
                // The result of "pointer - pointer" is the gap between them,
                // measured in elements.
                let expr = gen_promoted_binary_expr(lhs, rhs, ir::OP::Minus);
                let scale = sizeof(&basic_ty);
                let mut scaled_expr = scale_expr(expr, scale, ir::OP::Div);
                // @Note: In reality, the data type of the result is implementation dependent.
                // We intentionally hardcode it to be long for convenience.
                scaled_expr.ty = Type::Long;
                scaled_expr
            } else {
                gen_promoted_binary_expr(lhs, rhs, ir::OP::Minus)
            }
        }
        _ => {
            gen_promoted_binary_expr(lhs, rhs, op)
        }
    }
}

fn usual_arithmatic_conversion(lhs: ir::Expr, rhs: ir::Expr) -> (ir::Expr, ir::Expr) {
    let common_type = get_common_type(&lhs.ty, &rhs.ty);
    let casted_lhs = cast(lhs, &common_type);
    let casted_rhs = cast(rhs, &common_type);
    return (casted_lhs, casted_rhs);
}

fn get_common_type(lhs_type: &Type, rhs_type: &Type) -> Type {
    match lhs_type {
        Pointer_To(pointee_type) => return (pointer_to(pointee_type)),
        ArrayOf(element_type, _) => return (pointer_to(element_type)),
        _ => (),
    }

    if sizeof(lhs_type) == 8 || sizeof(rhs_type) == 8 {
        return Type::Long;
    }
    return Type::Int;
}

fn gen_promoted_binary_expr(lhs: ir::Expr, rhs: ir::Expr, op: ir::OP) -> ir::Expr {
    let span = Span{
        start_index: lhs.span.start_index,
        end_index: rhs.span.end_index,
    };
    let (lhs, rhs) = usual_arithmatic_conversion(lhs, rhs);
    let mut the_type = lhs.ty.clone();
    if matches!(op, ir::OP::Compare(..)) {
        the_type = Type::Int;
    }
    let new_expr_content = ir::ExprType::Binary(Box::new(lhs), Box::new(rhs), op);
    ir::Expr {
        content: new_expr_content,
        ty: the_type,
        span,
    }
}

fn gen_deref_expr(expr: ir::Expr) -> ir::Expr {
    let new_expr_content = ir::ExprType::Deref(Box::new(expr.clone()));
    let dereferenced_type = match &expr.ty {
        Pointer_To(pointee_type) => {
            if **pointee_type == Void {
                print_error_at(expr.span, "Hey bro no, you are trying to dereference a void pointer!");
                exit(1);
            }
            *pointee_type.clone()
        },
        ArrayOf(element_type, _) => *element_type.clone(),
        _ => {
            print_error_at(expr.span, "unable to generate deference of this expression, because it is
            nither a pointer nor array.");
            exit(1);
        },
    };
    ir::Expr {
        content: new_expr_content,
        ty: dereferenced_type,
        span: expr.span,
    }
}

fn gen_addr_of_expr(expr: ir::Expr) -> ir::Expr {
        let span = expr.span;
        let ty = pointer_to(&expr.ty);
        let content = ir::ExprType::AddrOf(Box::new(expr));
        ir::Expr{content, ty, span}
}

fn tokenkind_to_op(tokenkind: &TokenKind) -> ir::OP {
    use ir::OP;
    match tokenkind {
        Plus | PlusAssignment => OP::Plus,
        Minus | MinusAssignment => OP::Minus,
        Mul | MulAssignment => OP::Mul,
        Div | DivAssignment => OP::Div,
        Eq => OP::Compare(ir::CompareToken::Eq),
        Neq => OP::Compare(ir::CompareToken::Neq),
        LT => OP::Compare(ir::CompareToken::LT),
        LE => OP::Compare(ir::CompareToken::LE),
        GT => OP::Compare(ir::CompareToken::GT),
        GE => OP::Compare(ir::CompareToken::GE),
        _ => {
            println!("unknown operator");
            exit(1);
        }
    }
}

fn create_global_obj(name: &str, base_type: &Type) -> Obj {
    let mut cur_type = base_type.clone();
    let mut size: usize = sizeof(base_type);
    let obj = Obj{name: name.to_string(), ty: base_type.clone(), offset: 0, is_global: true};
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

pub fn align_to(n: usize, align: usize) -> usize {
    let extra = n % align;
    let base = n - extra;
    match extra {
        0 => base,
        _ => base + align,
    }
}
