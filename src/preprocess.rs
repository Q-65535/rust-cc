use crate::common::*;
use crate::lex::*;
use crate::parse::*;
use crate::ExprType::*;
use TokenKind::*;
use std::collections::HashMap;
use std::process::exit;
use crate::FILE_RECORDS;

/*
#[derive(Debug, Clone, Default)]
pub enum PP_Unit {
    Token(Box<Token>),
    If_Group(Box<If_Group>),
}

#[derive(Debug, Clone, Default)]
struct If_Group {
    condition: i64,
    then: Vec<PP_Unit>,
    otherwise: Option<Box::<If_Group>>,
}
*/

#[derive(Debug, Clone, Default)]
pub struct Preprocessor {
    nested_if_layer: usize,
}

impl Preprocessor {
    pub fn new() -> Self {
        Preprocessor {
            nested_if_layer: 0,
        }
    }
    
    pub fn inner_preprocess(&mut self, tokens: &[Token]) -> Vec<Token> {
        let mut preprocessed_tokens = Vec::new();
        let mut index = 0;
        while index < tokens.len() {
            match &tokens[index].kind {
                Punct("#") => {
                    if tokens[index].at_bol {
                        index += 1;
                        if tokens[index].at_bol {
                            continue;
                        }

                        let directive = match &tokens[index].kind {
                            LexIdent(name) => name.as_str(),
                            Keyword(name) => name,
                            _ => {
                                let error_description = format!("the token follwing '#' is invalid: {:?}", tokens[index].kind);
                                report_preprocess_error(tokens[index].span, &error_description);
                                exit(1);
                            }
                        };
                        let span = tokens[index].span;
                        if directive == "include" {
                            index += 1;
                            if let StringLiteral(file_name_bytes) = &tokens[index].kind {
                                let cur_file_path_buf = {
                                    let file_index = tokens[index].span.file_index;
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
                                let include_tokens = &include_tokens[..include_tokens.len()-1]; // get rid of Eof
                                let mut preprocessed_include_tokens = self.inner_preprocess(include_tokens);
                                preprocessed_tokens.append(&mut preprocessed_include_tokens);

                                index += 1;
                                // Skip extra tokens after #include "..".
                                while !tokens[index].at_bol {
                                    let error_description = "Warning: Extra token.";
                                    let error_info = warning_span(tokens[index].span, error_description);
                                    println!("{}", error_info);
                                    index += 1;
                                }
                            }
                        } else if matches!(directive, "if" | "elif") {
                            if directive == "elif" {
                                if self.nested_if_layer == 0 {
                                    let error_description = "WTF are you doing; we are not inside any #if group.";
                                    report_preprocess_error(span, error_description);
                                }
                            }
                            self.nested_if_layer += 1;
                            index += 1;
                            if tokens[index].at_bol {
                                let error_description = "No expression found after #if";
                                report_preprocess_error(span, error_description);
                            }

                            let expr_start_index = index;
                            while !tokens[index].at_bol {
                                index += 1;
                            }
                            let raw_expr_tokens = &tokens[expr_start_index..index];
                            let expanded_expr_tokens = self.expand(raw_expr_tokens);

                            let mut parser = Parser::new(expanded_expr_tokens);
                            let expr = match parser.parse_expr(Precedence::Lowest, Associativity::Left_To_Right) {
                                Ok(result) => result,
                                Err(e) => {
                                    println!("{}", e);
                                    exit(1);
                                }
                            };

                            let num = eval_integer_const(&expr);
                            if num != 0 {
                                // @Duplication_a
                                let block_start_index = index;
                                self.skip_cur_block(&mut index, tokens);
                                let skipped_block = &tokens[block_start_index..index];
                                let mut preprocessed_block = self.inner_preprocess(skipped_block);
                                preprocessed_tokens.append(&mut preprocessed_block);
                                self.skip_cur_if_layer(&mut index, tokens);
                                continue;
                            } else {
                                self.skip_cur_block(&mut index, tokens);
                                continue;
                            }
                        } else if directive == "else" {
                            index += 1;
                            if !tokens[index].at_bol {
                                let error_description = "Extra token(s) after #else.";
                                report_preprocess_error(tokens[index].span, error_description);
                            }

                            // @Duplication_a
                            let block_start_index = index;
                            self.skip_cur_block(&mut index, tokens);
                            let skipped_block = &tokens[block_start_index..index];
                            let mut preprocessed_block = self.inner_preprocess(skipped_block);
                            preprocessed_tokens.append(&mut preprocessed_block);
                            self.skip_cur_if_layer(&mut index, tokens);
                            continue;
                        } else if directive == "ifdef" {
                            todo!();
                        } else if directive == "ifndef" {
                            todo!();
                        } else if directive == "endif" {
                            if self.nested_if_layer > 0 {
                                index += 1;
                                self.nested_if_layer -= 1;
                            } else {
                                let error_description = "extra endif directive";
                                report_preprocess_error(tokens[index].span, error_description);
                            }
                        }
                    } else {
                        let error_description = "Currently only support '#' be at the begining of a line.";
                        report_preprocess_error(tokens[index].span, error_description);
                    }
                }
                _ => {
                    preprocessed_tokens.push(tokens[index].clone());
                    index += 1;
                }
            }
        }
        return preprocessed_tokens;
    }

    pub fn skip_cur_block(&mut self, index: &mut usize, tokens: &[Token]) {
        let start_if_layer = self.nested_if_layer;
        let start_span = tokens[*index].span;
        loop {
            if *index >= tokens.len() {
                let error_description = format!("starting from this point, missing #endif.");
                report_preprocess_error(start_span, &error_description);
            }

            if tokens[*index].at_bol && tokens[*index].kind == Punct("#") {
                *index += 1;
                if let LexIdent(directive) = &tokens[*index].kind {
                    if matches!(directive.as_str(), "elif" | "else" | "endif") {
                        if self.nested_if_layer == start_if_layer {
                            // Go back to # punctuator, so that preprocess can handle after this function returns.
                            *index -= 1;
                            return;
                        } else {
                            if matches!(directive.as_str(), "endif") {
                                self.nested_if_layer -= 1;
                            }
                            *index += 1;
                        }
                    } else if matches!(directive.as_str(), "if" | "ifdef" | "ifndef") {
                        self.nested_if_layer += 1;
                        *index += 1;
                    }
                }
            } else {
                *index += 1;
            }
        }
    }

    pub fn skip_cur_if_layer(&mut self, index: &mut usize, tokens: &[Token]) {
        let start_if_layer = self.nested_if_layer;
        let start_span = tokens[*index].span;
        loop {
            if *index >= tokens.len() {
                let error_description = format!("starting from this point, missing #endif.");
                report_preprocess_error(start_span, &error_description);
            }

            if tokens[*index].at_bol && tokens[*index].kind == Punct("#") {
                *index += 1;
                if let LexIdent(directive) = &tokens[*index].kind {
                    if matches!(directive.as_str(), "endif") {
                        if self.nested_if_layer == start_if_layer {
                            // Jump over #endif.
                            *index += 1;
                            return;
                        } else {
                            if matches!(directive.as_str(), "endif") {
                                self.nested_if_layer -= 1;
                            }
                            *index += 1;
                        }
                    } else if matches!(directive.as_str(), "if" | "ifdef" | "ifndef") {
                        self.nested_if_layer += 1;
                        *index += 1;
                    }
                }
            } else {
                *index += 1;
            }
        }
    }

    pub fn expand(&mut self, raw_tokens: &[Token]) -> Vec<Token> {
        let mut expanded_tokens = Vec::new();
        // @Temporary: Currenly just copy.
        for token in raw_tokens {
            expanded_tokens.push(token.clone());
        }
        return expanded_tokens;
    }
}

pub fn preprocess(tokens: &[Token]) -> Vec<Token> {
    let mut preprocessor = Preprocessor::new();
    return preprocessor.inner_preprocess(tokens);
}

// @Pending: How to handle usual arithmatic conversion?
fn eval_integer_const(expr: &Expr) -> i64 {
    match &expr.content {
        Int_Const(n) => *n,
        Long_Const(n) => *n,
        UInt_Const(n) => *n,
        ULong_Const(n) => *n,

        Binary(lhs, rhs, op) => {
            let left_num = eval_integer_const(lhs);
            let right_num = eval_integer_const(rhs);
            match op {
                Punct("+") => left_num + right_num,
                Punct("-") => left_num - right_num,
                Punct("*") => left_num * right_num,
                Punct("/") => left_num / right_num,
                Punct("%") => left_num % right_num,
                Punct("&") => left_num & right_num,
                Punct("^") => left_num & right_num,
                Punct("|") => left_num | right_num,
                Punct("<<") => left_num << right_num,
                Punct(">>") => left_num >> right_num,
                Punct("==") => (left_num == right_num) as i64,
                Punct("!=") => (left_num != right_num) as i64,
                // @TODO Should consider signed or unsigned for comparasion operators.
                Punct("<") => (left_num < right_num) as i64,
                Punct("<=") => (left_num <= right_num) as i64,
                Punct(">") => (left_num > right_num) as i64,
                Punct(">=") => (left_num >= right_num) as i64,

                Punct("&&") => {
                    if left_num != 0 && right_num != 0 {
                        return 1;
                    } else {
                        return 0;
                    }
                }
                Punct("||") => {
                    if left_num == 0 && right_num == 0 {
                        return 0;
                    } else {
                        return 1;
                    }
                }
                _ => {
                    let error_description = format!("Unable to handle this binary operator at preprocess phase: {:?}", op);
                    report_preprocess_error(expr.span, &error_description);
                    exit(1);
                }
            }
        }
        // -
        Neg(expr) => {
            let num = eval_integer_const(expr);
            return -num;
        }
        // !
        Not(expr) => {
            let num = eval_integer_const(expr);
            if num == 0 {
                return 1;
            } else {
                return 0;
            }
        }
        // ~
        BitNot(expr) => {
            let num = eval_integer_const(expr);
            return !num;
        }
        Conditional(cond_expr, then_expr, otherwise_expr) => {
            let cond_num = eval_integer_const(cond_expr);
            if cond_num != 0 {
                let then_num = eval_integer_const(then_expr);
                return then_num;
            } else {
                let otherwise_num = eval_integer_const(otherwise_expr);
                return otherwise_num;
            }
        }
        Paren(expr) => eval_integer_const(expr),
        bad_expr => {
            let error_description = format!("Unable to handle this expression at preprocess phase: {:?}", bad_expr);
            report_preprocess_error(expr.span, &error_description);
            exit(1);
        }
    }
}

fn report_preprocess_error(span: Span, error_description: &str) {
    let mut prefixed_error_description = String::new();
    prefixed_error_description.push_str("Preprocess error: ");
    prefixed_error_description.push_str(error_description);
    let error_info = error_span(span, &prefixed_error_description);
    println!("{}", error_info);
    exit(1);
}
