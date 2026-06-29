use crate::analyze::{self, Type::*, *};
use crate::common::{self, *};

#[derive(Debug, Clone, PartialEq)]
pub enum StmtType {
    Ex(Expr),
    Return(Expr),
    Block(Vec<StmtType>),
    If {cond: Expr, then: Box<StmtType>, otherwise: Option<Box<StmtType>>},
    For {init: Option<Expr>, cond: Option<Expr>, inc: Option<Expr>, then: Box<StmtType>},
}
use StmtType::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Number(i32),
    Binary(Box<Expr>, Box<Expr>, OP),
    Assign(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Deref(Box<Expr>),
    AddrOf(Box<Expr>),
    RequestStructMember(Box<Expr>, i32),
    // SymbolRef(Obj),
    Ident(Obj),
    ArrayIndexing(Box<Expr>, Vec<Expr>),
    CommaExpression(Box<Expr>, Box<Expr>),
    FunCall(Box<Expr>, Vec<Expr>),
    StmtExpr(Vec<StmtType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub content: ExprType,
    pub ty: Type,
    pub span: Span,
}

impl Expr {
    pub fn is_integer(&self) -> bool {
        if let Type::Int = &self.ty {
            true
        } else {
            false
        }
    }
    // @Naming: is_ptr_or_array
    pub fn is_pointer_or_array(&self) -> bool {
        match &self.ty {
            Pointer_To(_) | ArrayOf(_, _) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub param_names: Vec<String>,
    pub stmts: Vec<StmtType>,
    pub scope_tracker: ScopeTracker,
    pub stack_size: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub obj: Obj,
    pub init_value: Option<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tagged_Struct {
    pub tag_name: String,
    pub the_struct: Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub members: Vec<Member>,
    pub size: i32,
    pub align: i32,
}

impl Struct {
    pub fn has_member(&self, name: &String) -> bool {
        for m in &self.members {
            if m.name == *name {
                return true;
            }
        }
        return false;
    }
    pub fn get_member(&self, name: &String) -> Result<Member, String> {
        for m in &self.members {
            if m.name == *name {
                return Ok(m.clone());
            }
        }
        let err_msg = format!("semantic error: cannot find struct member {:?}", name);
        return Err(err_msg);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub ty: Type,
    pub name: String,
    pub offset: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RequestStructMember {
    pub ty: Type,
    pub name: String,
    pub offset: i32,
}

pub struct AnalyzedProgram {
    pub afuns: Vec<Function>,
    pub global_decls: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OP {
    Plus,
    Minus,
    Mul,
    Div,
    Compare(CompareToken),
}

#[derive(PartialEq, Clone, Debug)]
pub enum CompareToken {
    Eq,
    Neq,
    LT,
    LE,
    GT,
    GE,
}
use CompareToken::*;