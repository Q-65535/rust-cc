use crate::common::*;
use crate::lex::*;
use TokenKind::*;
use crate::SRC;
use std::collections::HashMap;
use std::process::exit;

#[derive(Debug, Clone, Default)]
pub struct Preprocessor {}

impl Preprocessor {}

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
    let mut preprocessed_tokens = Vec::new();
    for token in &tokens {
        match &token.kind {
            Punct("#") => {
                if token.at_bol {
                    continue;
                } else {
                    println!("invalid preprocessor directive");
                    exit(1);
                }
            }
            _ => {
                preprocessed_tokens.push(token.clone());
            }
        }
    }
    return preprocessed_tokens;
}

fn report_preprocess_error(span: Span, error_info: &str) {
}
