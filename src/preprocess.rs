use crate::common::*;
use crate::lex::*;
use TokenKind::*;
use std::collections::HashMap;
use std::process::exit;
use crate::FILE_RECORDS;

#[derive(Debug, Clone, Default)]
pub struct Preprocessor {}

impl Preprocessor {}

pub fn preprocess(mut tokens: Vec<Token>) -> Vec<Token> {
    let mut preprocessed_tokens = Vec::new();
    let mut index = 0;
    while index < tokens.len() {
        let cur_token = &tokens[index];
        match &cur_token.kind {
            Punct("#") => {
                if cur_token.at_bol {
                    index += 1;
                    if let LexIdent(directive) = &tokens[index].kind {
                        index += 1;
                        if let StringLiteral(file_name_bytes) = &tokens[index].kind {

                            let cur_path_buf = {
                                let file_index = cur_token.span.file_index;
                                let mut file_records = FILE_RECORDS.lock().unwrap();
                                file_records[file_index].path.clone()
                            };
                            let mut new_path_buf = cur_path_buf.parent().unwrap().to_path_buf();
                            let file_name = String::from_utf8(file_name_bytes.clone()).unwrap();
                            new_path_buf.push(file_name);

                            let final_path_name = new_path_buf.to_str().unwrap();
                            let file = load_file(final_path_name);
                            let file_index = {
                                let mut file_records = FILE_RECORDS.lock().unwrap();
                                file_records.push(file);
                                file_records.len() - 1
                            };
                            let mut lexer = Lexer::new(file_index);
                            let mut tokens = lexer.lex();
                            // @Temporary: better way to handle it
                            let mut tokens = tokens[..tokens.len()-1].to_vec(); // get rid of Eof
                            let mut tokens = preprocess(tokens);
                            
                            preprocessed_tokens.append(&mut tokens);

                            index += 1;
                        }
                    }
                } else {
                    println!("invalid preprocessor directive");
                    exit(1);
                }
            }
            _ => {
                preprocessed_tokens.push(cur_token.clone());
                index += 1;
            }
        }
    }
    return preprocessed_tokens;
}

fn report_preprocess_error(span: Span, error_info: &str) {
}

