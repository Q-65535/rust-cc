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
    Token(Token),
    Nil,
}

fn main() {
    let tok = Token {
        kind: TokenKind::TkNum,
        next: Box::new(Node::Nil),
        val: 3,
        index: 0,
        len: 1,
    };
    let tok_node = Node::Token(tok);
    if let Node::Token(tn) = tok_node {
        println!("add ${}, %rax\n", tn.val);
    }
    let src = String::from("100+99-10+123455");
    tokenize(&src);
}

fn tokenize(src: &String) {
    let mut index: usize = 0;
    while index < src.len() {
        if let Some(c) = src.chars().nth(index) {
            if c == '+' || c == '-' {
                println!("{c}");
                index += 1;
            } else if c <= '9' && c >= '0' {
                let (len, int_str) = read_int(src[index..], index);
                index += len;
                println!("{int_str}");
            }
        } else {
            println!("index out of bounds");
        }
    }
}

fn read_int(src: &str, mut index: usize) -> (usize, &str) {
    let start_index = index;
    for c.src.chars().enumerate() {
        
    }
    loop {
        if let Some(c) = src.chars().nth(index) {
            if c <= '9' && c >= '0' {
                index += 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }
    return (index - start_index, &src[start_index..index]);
}