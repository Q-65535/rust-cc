use crate::analyze::{self, Type::*, *};
use crate::lex::{Location};

#[derive(Debug, Clone)]
pub enum StmtType {
    Ex(Expr),
    Return(Expr),
    Block(Vec<StmtType>),
    If {cond: Expr, then: Box<StmtType>, otherwise: Option<Box<StmtType>>},
    For {init: Option<Expr>, cond: Option<Expr>, inc: Option<Expr>, then: Box<StmtType>},
}
use StmtType::*;

// #[derive(Debug, Clone)]
// pub enum BlockItem {
//     Stmt(StmtType),
//     Decl(Declaration),
// }

// #[derive(Debug, Clone)]
// pub struct Declaration {
//     pub decl_spec: DeclarationSpecifier,
//     pub declarators: Vec<Declarator>,
// }

// #[derive(Debug, Clone)]
// pub struct Declarator {
//     pub star_count: i32,
//     pub name: String,
//     pub suffix: Option<DeclaratorSuffix>,
//     pub location: Location,
// }

// #[derive(Debug, Clone)]
// pub enum DeclaratorSuffix {
//     ArrayLen(Vec<i32>),
//     FunParam(Vec<Parameter>),
// }
// use DeclaratorSuffix::*;


// #[derive(Debug, Clone)]
// pub struct Parameter {
//     pub decl_spec: DeclarationSpecifier,
//     pub declarator: Declarator,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Number(i32),
    Binary(Box<Expr>, Box<Expr>, OP),
    Assign(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Deref(Box<Expr>),
    AddrOf(Box<Expr>),
    Ident(String),
    ArrayIndexing(Box<Expr>, Vec<Expr>),
    FunCall(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub content: ExprType,
    pub ty: Type,
    pub location: Location,
}

impl Expr {
    pub fn is_integer(&self) -> bool {
        if let TyInt = &self.ty {
            true
        } else {
            false
        }
    }
    // @Naming: is_ptr_or_array
    pub fn is_ptr(&self) -> bool {
        match &self.ty {
            TyPtr(_) | ArrayOf(_, _) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub param_names: Vec<String>,
    pub stmts: Vec<StmtType>,
    pub sbl_table: SblTable,
    pub stack_size: i32,
}

pub struct AnalyzedProgram {
    pub afuns: Vec<Function>,
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