use std::io::{self, Write};

enum TokenKind {
    TkPunct, // Punctuators
    TkNum,   // Numeric literals
    TkEof,   // End-of-file markers
}

struct Token {
    kind: TokenKind,
    next: Box<Node>,
    val: i32,
    index: i32,
    len: i32,
}

enum Node {
    Token{
        kind: TokenKind,
        next: Box<Node>,
        val: i32,
        index: i32,
        len: i32,
    },
    Nil,
}

fn main() {
    let tok = Node::Token {
        kind: TokenKind::TkNum,
        next: Box::new(Node::Nil),
        val: 1,
        index: 0,
        len: 4,
    };

    if let Node::Token { kind, next, val, index, len } = tok {
        println!("  add ${val}, %rax\n");
    }
}