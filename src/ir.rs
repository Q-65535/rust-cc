use crate::analyze::{self, Type::*, *};
use crate::common::{self, *};

#[derive(Debug, Clone, PartialEq)]
pub enum StmtType {
    Ex(Expr),
    Return(Expr),
    Block(Vec<StmtType>),
    If {cond: Expr, then: Box<StmtType>, otherwise: Option<Box<StmtType>>},
    For {init: Vec<StmtType>, cond: Option<Expr>, inc: Option<Expr>, then: Box<StmtType>, end_label: String, continue_point_label: String},
    Switch{switch_case_info: Switch_Case, body: Box<StmtType>, end_label: String},
    CaseStmt{unique_label: String, stmt: Box<StmtType>},
    Goto(String),
    LabeledStmt(String, Box<StmtType>),
}
use StmtType::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Switch_Case {
    pub target_expr: Expr,
    pub cases: Vec<Case>,
    pub default_label: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub matching_value: i64,
    pub unique_label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Natural_Number(u64),
    Binary(Box<Expr>, Box<Expr>, OP),
    Assign(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    BitNot(Box<Expr>),
    Deref(Box<Expr>),
    AddrOf(Box<Expr>),
    RequestStructMember(Box<Expr>, usize),
    Object(Obj),
    ArrayIndexing(Box<Expr>, Vec<Expr>),
    CommaExpression(Box<Expr>, Box<Expr>),
    FunCall(Box<Expr>, Vec<Expr>),
    StmtExpr(Vec<StmtType>),
    Cast(Box<Expr>, Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub content: ExprType,
    pub ty: Type,
    pub span: Span,
}

impl Expr {
    // @Refactor: duplicate functions
    pub fn is_integer(&self) -> bool {
        return analyze::is_integer(&self.ty);
    }
    pub fn is_pointer_or_array(&self) -> bool {
        return analyze::is_pointer_or_array(&self.ty);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Obj>,
    pub stmts: Vec<StmtType>,
    pub stack_size: usize,
    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub obj: Obj,
    pub init_value: Option<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub members: Vec<Member>,
    pub size: usize,
    pub align: usize,
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
    pub offset: usize,
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

// binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum OP {
    Plus,
    Minus,
    Mul,
    Div,
    Modulus,

    // compares
    Eq,
    Neq,
    LT,
    LE,
    GT,
    GE,

    // bitwise
    BitAnd,
    BitXOR,
    BitOR,
}

impl OP {
    pub fn is_compare(&self) -> bool {
        matches!(self, OP::Eq | OP::Neq | OP::LT | OP::LE | OP::GT | OP::GE)
    }
}
