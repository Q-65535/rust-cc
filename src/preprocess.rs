use crate::common::*;
use crate::lex::*;
use crate::parse::*;
use crate::ExprType::*;
use TokenKind::*;
use std::collections::HashMap;
use std::process::exit;
use crate::FILE_RECORDS;

#[derive(Debug, Clone)]
struct PP_Context<'a> {
    cur_if_layer: usize,
    tokens: &'a [Token],
    cur_index: usize,
    eof_token: Token,
}

impl<'a> PP_Context<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let eof_span = if tokens.len() > 0 {
            let last_token_span = tokens[tokens.len() - 1].span;
            Span {
                file_index: last_token_span.file_index,
                start_index: last_token_span.end_index + 1,
                end_index: last_token_span.end_index + 1,
            }
        } else {
            Span {
                file_index: 0,
                start_index: 0,
                end_index: 0,
            }
        };
        PP_Context {
            cur_if_layer: 0,
            tokens,
            cur_index: 0,
            eof_token: gen_token(Eof, eof_span),
        }
    }

    pub fn cur_token(&self) -> &Token {
        if self.cur_index >= self.tokens.len() {
            return &self.eof_token;
        }
        return &self.tokens[self.cur_index];
    }

    pub fn peek_token(&self) -> &Token {
        if self.cur_index + 1 >= self.tokens.len() {
            return &self.eof_token;
        }
        return &self.tokens[self.cur_index+1];
    }

    // @Refactor: This just returns an Eof if reaching end of input.
    // The error info should be reported at where the error happens.
    pub fn eat_token(&mut self) {
        if self.cur_token().kind == Eof {
            let error_description = "Reaching end of input.";
            report_preprocess_error(self.cur_token().span, &error_description);
        }
        self.cur_index += 1;
    }

    pub fn cur_if_layer(&self) -> usize {
        return self.cur_if_layer;
    }

    pub fn enter_if(&mut self) {
        self.cur_if_layer += 1;
    }

    pub fn exit_if(&mut self) {
        debug_assert!(self.cur_if_layer != 0);
        self.cur_if_layer -= 1;
    }

    pub fn advance_to_next_line(&mut self) -> &[Token] {
        let begin_index = self.cur_index;
        while !self.cur_token().at_bol {
            self.eat_token();
        }
        let end_index = self.cur_index;
        return &self.tokens[begin_index..end_index];
    }
}

#[derive(Debug, Clone, Default)]
pub struct Preprocessor {
}

impl Preprocessor {
    pub fn new() -> Self {
        Preprocessor {
        }
    }


    pub fn inner_preprocess(&mut self, tokens: &[Token]) -> Vec<Token> {
        let mut preprocessed_tokens = Vec::new();
        let mut index = 0;
        let mut context = PP_Context::new(tokens);

        while context.cur_token().kind != Eof {
            match &context.cur_token().kind {
                Punct("#") => {
                    if !context.cur_token().at_bol {
                        let error_description = "'#' shouldn't be here, because \
                        '#' is only allowed to be at the begining of a line or in macro replacement-list.";
                        report_preprocess_error(context.cur_token().span, error_description);
                    }
                    context.eat_token();
                    if context.cur_token().at_bol {
                        continue;
                    }

                    let directive = match &context.cur_token().kind {
                        LexIdent(name) => name.as_str(),
                        Keyword(name) => name,
                        _ => {
                            let error_description = format!("the token follwing '#' is invalid: {:?}", context.cur_token().kind);
                            report_preprocess_error(context.cur_token().span, &error_description);
                        }
                    };
                    let span = context.cur_token().span;
                    if directive == "include" {
                        context.eat_token();
                        if let StringLiteral(file_name_bytes) = &context.cur_token().kind {
                            let cur_file_path_buf = {
                                let file_index = context.cur_token().span.file_index;
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
                            let mut preprocessed_include_tokens = self.inner_preprocess(&include_tokens);
                            preprocessed_tokens.append(&mut preprocessed_include_tokens);

                            context.eat_token();
                            // Skip extra tokens after #include "..".
                            while !context.cur_token().at_bol && context.cur_token().kind != Eof {
                                let error_description = "Warning: Extra token.";
                                let error_info = warning_span(context.cur_token().span, error_description);
                                println!("{}", error_info);
                                context.eat_token();
                            }
                        }
                    } else if matches!(directive, "if" | "elif") {
                        if directive == "elif" {
                            // @Smell: just access the field, don't use function.
                            if context.cur_if_layer() == 0 {
                                let error_description = "WTF are you doing; we are not inside any #if group.";
                                report_preprocess_error(span, error_description);
                            }
                        }
                        context.enter_if();
                        context.eat_token();
                        if context.cur_token().at_bol {
                            let error_description = "No expression found after #if";
                            report_preprocess_error(span, error_description);
                        }

                        let raw_expr_tokens = context.advance_to_next_line();
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
                            let skipped_block = skip_cur_block(&mut context);
                            let mut preprocessed_block = self.inner_preprocess(skipped_block);
                            preprocessed_tokens.append(&mut preprocessed_block);
                            skip_cur_if_layer(&mut context);
                            continue;
                        } else {
                            skip_cur_block(&mut context);
                            continue;
                        }
                    } else if directive == "else" {
                        context.eat_token();
                        if !context.cur_token().at_bol {
                            let error_description = "Extra token(s) after #else.";
                            report_preprocess_error(context.cur_token().span, error_description);
                        }

                        // @Duplication_a
                        let skipped_block = skip_cur_block(&mut context);
                        let mut preprocessed_block = self.inner_preprocess(skipped_block);
                        preprocessed_tokens.append(&mut preprocessed_block);
                        skip_cur_if_layer(&mut context);
                        continue;
                    } else if directive == "ifdef" {
                        todo!();
                    } else if directive == "ifndef" {
                        todo!();
                    } else if directive == "endif" {
                        if context.cur_if_layer() > 0 {
                            context.eat_token();
                            context.exit_if();
                        } else {
                            let error_description = "extra endif directive";
                            report_preprocess_error(context.cur_token().span, error_description);
                        }
                    }
                }
                _ => {
                    preprocessed_tokens.push(context.cur_token().clone());
                    context.eat_token();
                }
            }
        }

        return preprocessed_tokens;
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
        }
    }
}

fn report_preprocess_error(span: Span, error_description: &str) -> ! {
    let mut prefixed_error_description = String::new();
    prefixed_error_description.push_str("Preprocess error: ");
    prefixed_error_description.push_str(error_description);
    let error_info = error_span(span, &prefixed_error_description);
    println!("{}", error_info);
    exit(1);
}

pub fn skip_cur_block<'ctx, 'tokens>(context: &'ctx mut PP_Context<'tokens>) -> &'ctx [Token] {
    let start_index = context.cur_index;
    let start_if_layer = context.cur_if_layer;
    loop {
        if context.cur_token().at_bol && context.cur_token().kind == Punct("#") {
            context.eat_token();
            if let LexIdent(directive) = &context.cur_token().kind {
                if matches!(directive.as_str(), "elif" | "else" | "endif") {
                    if context.cur_if_layer == start_if_layer {
                        // Go back to # punctuator, so that preprocess can handle after this function returns.
                        context.cur_index -= 1;
                        break;
                    } else {
                        if matches!(directive.as_str(), "endif") {
                            context.cur_if_layer -= 1;
                        }
                        context.eat_token();
                    }
                } else if matches!(directive.as_str(), "if" | "ifdef" | "ifndef") {
                    context.cur_if_layer += 1;
                    context.eat_token();
                }
            }
        } else {
            context.eat_token();
        }
    }
    return &context.tokens[start_index..context.cur_index];
}

pub fn skip_cur_if_layer<'ctx, 'tokens>(context: &'ctx mut PP_Context<'tokens>) -> &'ctx [Token] {
    let start_index = context.cur_index;
    let start_if_layer = context.cur_if_layer;
    loop {
        if context.cur_token().at_bol && context.cur_token().kind == Punct("#") {
            context.eat_token();
            if let LexIdent(directive) = &context.cur_token().kind {
                if matches!(directive.as_str(), "endif") {
                    if context.cur_if_layer == start_if_layer {
                        // Go back to # punctuator, so that preprocess can handle after this function returns.
                        context.cur_index -= 1;
                        break;
                    } else {
                        if matches!(directive.as_str(), "endif") {
                            context.cur_if_layer -= 1;
                        }
                        context.eat_token();
                    }
                } else if matches!(directive.as_str(), "if" | "ifdef" | "ifndef") {
                    context.cur_if_layer += 1;
                    context.eat_token();
                }
            }
        } else {
            context.eat_token();
        }
    }
    return &context.tokens[start_index..context.cur_index];
}