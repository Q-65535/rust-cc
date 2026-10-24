use crate::common::*;
use crate::lex::*;
use TokenKind::*;
use std::collections::HashMap;
use std::process::exit;
use crate::FILE_RECORDS;

#[derive(Debug, Clone, Default)]
pub struct Preprocessor<'a> {
    tokens: &'a [Token],
    cur_index: usize,
}

impl<'a> Preprocessor<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Preprocessor {
            tokens,
            cur_index: 0,
        }
    }
    
    fn bump(&mut self) {
        self.cur_index += 1;
    }

    pub fn cur_token(&self) -> &Token {
        &self.tokens[self.cur_index]
    }
    
    pub fn preprocess(&mut self) -> Vec<Token> {
        let mut preprocessed_tokens = Vec::new();
        while self.cur_index < self.tokens.len() {
            match self.cur_token().kind {
                Punct("#") => {
                    if self.cur_token().at_bol {
                        self.bump();
                        if self.cur_token().at_bol {
                            continue;
                        }
                        if let LexIdent(directive) = &self.cur_token().kind {
                            if directive.as_ref() == "include" {
                                self.bump();
                                if let StringLiteral(file_name_bytes) = &self.cur_token().kind {
                                    let cur_file_path_buf = {
                                        let file_index = self.cur_token().span.file_index;
                                        let mut file_records = FILE_RECORDS.lock().unwrap();
                                        file_records[file_index].path.clone()
                                    };
                                    let cur_dir_path_buf = cur_file_path_buf.parent().unwrap().to_path_buf();
                                    let include_file_name = String::from_utf8(*file_name_bytes.clone()).unwrap();
                                    let include_file_path = cur_dir_path_buf.join(include_file_name);
    
                                    let include_file_path_str = include_file_path.to_str().unwrap();
                                    let file = load_file(include_file_path_str);
                                    let mut include_tokens: Vec<Token>;
                                    {
                                        let mut file_records = FILE_RECORDS.lock().unwrap();
                                        file_records.push(file);
                                        let content_ref: &str = &file_records.last().unwrap().content;
                                        let mut lexer = Lexer::new(file_records.len()-1, content_ref);
                                        include_tokens = lexer.lex();
                                    }
                                    // @Temporary: better way to handle it
                                    let mut include_tokens = include_tokens[..include_tokens.len()-1].to_vec(); // get rid of Eof
                                    let mut inner_preprocessor = Preprocessor::new(&include_tokens);
                                    let mut preprocessed_include_tokens = inner_preprocessor.preprocess();
                                    preprocessed_tokens.append(&mut preprocessed_include_tokens);
    
                                    self.bump();
                                    // Skip extra tokens after #include "..".
                                    while !self.cur_token().at_bol {
                                        let error_description = "Warning: Extra token.";
                                        let error_info = warning_span(self.cur_token().span, error_description);
                                        println!("{}", error_info);
                                        self.bump();
                                    }
                                }
                            } else if directive.as_ref() == "if" {
                                self.bump();
                                let raw_expr_tokens = self.read_tokens_till_end_of_line();
                                let expanded_expr_tokens = self.expand(raw_expr_tokens);
                                
                            } else if directive.as_ref() == "endif" {
                                todo!();
                            }
                        } else {
                            let error_description = "there is no directive following '#'";
                            report_preprocess_error(self.cur_token().span, error_description);
                        }
                    } else {
                        let error_description = "Currently only support '#' be at the begining of a line.";
                        report_preprocess_error(self.cur_token().span, error_description);
                    }
                }
                _ => {
                    preprocessed_tokens.push(self.cur_token().clone());
                    self.bump();
                }
            }
        }
        return preprocessed_tokens;
    }

    pub fn read_tokens_till_end_of_line(&mut self) -> &[Token] {
        debug_assert!(self.cur_token().at_bol);
        let start_index = self.cur_index;
        // @Temporary: Currenly just copy.
        while (!self.cur_token().at_bol) {
            self.bump();
        }
        let end_index_exclusive = self.cur_index;
        return &self.tokens[start_index..end_index_exclusive];
    }

    pub fn expand(&mut self, raw_tokens: &[Token]) -> Vec<Token> {
        let mut expanded_tokens = Vec::new();
        for token in raw_tokens {
            expanded_tokens.push(token.clone());
        }
        return expanded_tokens;
    }
}

fn report_preprocess_error(span: Span, error_description: &str) {
    let mut prefixed_error_description = String::new();
    prefixed_error_description.push_str("Preprocess errro: ");
    prefixed_error_description.push_str(error_description);
    let error_info = error_span(span, &prefixed_error_description);
    println!("{}", error_info);
    exit(1);
}

