use crate::common::*;
use crate::lex::*;
use TokenKind::*;
use crate::SRC;
use crate::error_span;
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
                    report_preprocess_error(token.span, "invalid preprocessor directive");
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
    let error_stage_info = "Preprocess error: ".to_string();
    let error_info = error_span(span, &(error_stage_info+error_info));
    println!("{}", error_info);
    exit(1);
}
