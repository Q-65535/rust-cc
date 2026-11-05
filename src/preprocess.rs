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
    if_layers: Vec<Span>,
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
            if_layers: Vec::new(),
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

    pub fn eat_token(&mut self) {
        self.cur_index += 1;
    }

    pub fn cur_if_layer(&self) -> usize {
        return self.if_layers.len();
    }

    pub fn cur_if_directive_position(&self) -> Span {
        return self.if_layers[self.if_layers.len() - 1];
    }

    pub fn enter_if(&mut self) {
        self.if_layers.push(self.cur_token().span);
    }

    pub fn exit_if(&mut self) {
        debug_assert!(self.if_layers.len() > 0);
        self.if_layers.pop();
    }

    pub fn eat_and_get_remainder_of_line(&mut self) -> &[Token] {
        let begin_index = self.cur_index;
        loop {
            if self.cur_token().at_bol {break};
            if self.cur_token().kind == Eof {break};
            self.eat_token();
        }
        let end_index = self.cur_index;
        return &self.tokens[begin_index..end_index];
    }
}

#[derive(Debug, Clone, Default)]
pub struct Preprocessor {
    pub macros: HashMap<String, Vec<Token>>,
}

impl Preprocessor {
    pub fn new() -> Self {
        Preprocessor {
            macros: HashMap::new(),
        }
    }

    // @TODO: To be implemented.
    pub fn is_defined(&self, name: &str) -> bool {
        return self.macros.contains_key(name);
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
                        break;
                    }
                    context.eat_token();
                    // A single # with no follwing stuff in the same line is just a null directive; we skip it.
                    if context.cur_token().at_bol {
                        continue;
                    }
                    
                    let directive = get_name(&context.cur_token().kind);
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
                        } else {
                            let error_description = "Currently Only support string format include path (\"path\"), so we don't understand this.";
                            report_preprocess_error(context.cur_token().span, error_description);
                        }
                    } else if matches!(directive, "if" | "elif") {
                        if directive == "elif" {
                            if context.cur_if_layer() == 0 {
                                let error_description = "WTF are you doing; we are not inside any #if group.";
                                report_preprocess_error(span, error_description);
                            }
                        }
                        if directive == "if" {
                            context.enter_if();
                        }
                        context.eat_token();
                        if context.cur_token().at_bol {
                            let error_description = "Nothing found after #if";
                            report_preprocess_error(span, error_description);
                            break;
                        }

                        let raw_expr_tokens = context.eat_and_get_remainder_of_line();
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
                            let skipped_block = eat_and_get_cur_block(&mut context);
                            let mut preprocessed_block = self.inner_preprocess(skipped_block);
                            preprocessed_tokens.append(&mut preprocessed_block);
                            eat_remaining_cur_if_layer(&mut context);
                            continue;
                        } else {
                            eat_and_get_cur_block(&mut context);
                            continue;
                        }
                    } else if directive == "else" {
                        if context.cur_if_layer() == 0 {
                            let error_description = "Not in any if-group.";
                            report_preprocess_error(context.cur_token().span, error_description);
                            break;
                        }
                        context.eat_token();
                        if !context.cur_token().at_bol {
                            let error_description = "Extra token(s) after #else.";
                            report_preprocess_error(context.cur_token().span, error_description);
                        }

                        let skipped_block = eat_and_get_cur_block(&mut context);
                        let mut preprocessed_block = self.inner_preprocess(skipped_block);
                        preprocessed_tokens.append(&mut preprocessed_block);
                        eat_remaining_cur_if_layer(&mut context);
                        continue;
                    } else if matches!(directive, "ifdef" | "ifndef") {
                        let is_ifdef = if directive == "ifdef" {true} else {false};
                        context.enter_if();
                        context.eat_token();
                        if context.cur_token().at_bol {
                            let error_description = "Nothing found after #ifdef";
                            report_preprocess_error(span, error_description);
                            break;
                        }

                        let name = get_name(&context.cur_token().kind);
                        let symbol_is_defined = self.is_defined(name);
                        let condition_satisfied = if is_ifdef {symbol_is_defined} else {!symbol_is_defined};

                        context.eat_token();
                        if !context.cur_token().at_bol {
                            let error_description = "Extra token(s).";
                            report_preprocess_error(context.cur_token().span, error_description);
                            break;
                        }
                        
                        if condition_satisfied {
                            let skipped_block = eat_and_get_cur_block(&mut context);
                            let mut preprocessed_block = self.inner_preprocess(skipped_block);
                            preprocessed_tokens.append(&mut preprocessed_block);
                            eat_remaining_cur_if_layer(&mut context);
                            continue;
                        } else {
                            eat_and_get_cur_block(&mut context);
                            continue;
                        }
                    } else if directive == "endif" {
                        if context.cur_if_layer() == 0 {
                            let error_description = "Not in any if-group";
                            report_preprocess_error(context.cur_token().span, error_description);
                            break;
                        }
                        context.eat_token();
                        context.exit_if();
                    } else if directive == "define" {
                        context.eat_token();
                        let remainder_of_line = context.eat_and_get_remainder_of_line();
                        if remainder_of_line.len() == 0 {
                            let error_description = "No macro name given in #define directive";
                            report_preprocess_error(span, error_description);
                            break;
                        }
                        let name = get_name(&remainder_of_line[0].kind);
                        if name == "" {
                            let error_description = "Macro name must be an identifier.";
                            report_preprocess_error(context.cur_token().span, error_description);
                            break;
                        }
                        let replacement_list = remainder_of_line[1..].to_vec();
                        self.macros.insert(name.to_string(), replacement_list);
                    } else if directive == "undef" {
                        context.eat_token();
                        if context.cur_token().at_bol {
                            let error_description = "No macro name given in #undef directive";
                            report_preprocess_error(span, error_description);
                            break;
                        }
                        let name = get_name(&context.cur_token().kind);
                        if name == "" {
                            let error_description = "Macro name must be an identifier.";
                            report_preprocess_error(context.cur_token().span, error_description);
                            break;
                        }
                        self.macros.remove(name);
                        context.eat_token();
                        if !context.cur_token().at_bol {
                            let error_description = "Extra token(s) after #undef.";
                            report_preprocess_error(context.cur_token().span, error_description);
                            break;
                        }
                    } else {
                        let error_description = "Unknown directive.";
                        report_preprocess_error(context.cur_token().span, error_description);
                    }
                }
                _ => {
                    if matches!(context.cur_token().kind, LexIdent(..) | Keyword(..)) {
                        let name = get_name(&context.cur_token().kind);
                        if let Some(replacement_list) = self.macros.get(name) {
                            preprocessed_tokens.append(&mut replacement_list.clone());
                            context.eat_token();
                            continue;
                        }
                    }
                    preprocessed_tokens.push(context.cur_token().clone());
                    context.eat_token();
                }
            }
        }

        return preprocessed_tokens;
    }



    pub fn expand(&self, raw_tokens: &[Token]) -> Vec<Token> {
        let mut expanded_tokens = Vec::new();
        for token in raw_tokens {
            if matches!(token.kind, LexIdent(..) | Keyword(..)) {
                let name = get_name(&token.kind);
                if let Some(replacement_list) = self.macros.get(name) {
                    expanded_tokens.append(&mut replacement_list.clone());
                    continue;
                }
            }
            expanded_tokens.push(token.clone());
        }
        return expanded_tokens;
    }
}

pub fn preprocess(tokens: &[Token]) -> Vec<Token> {
    let mut preprocessor = Preprocessor::new();
    return preprocessor.inner_preprocess(tokens);
}

pub fn eat_and_get_cur_block<'ctx, 'tokens>(context: &'ctx mut PP_Context<'tokens>) -> &'ctx [Token] {
    let start_index = context.cur_index;
    let target_if_layer = context.cur_if_layer();
    loop {
        if context.cur_token().kind == Eof {
            let error_description = "Can't find a matching #endif for this directive.";
            report_preprocess_error(context.cur_if_directive_position(), &error_description);
            break;
        }
        if context.cur_token().at_bol && context.cur_token().kind == Punct("#") {
            context.eat_token();
            let directive = get_name(&context.cur_token().kind);
            if matches!(directive, "elif" | "else" | "endif") {
                if context.cur_if_layer() == target_if_layer {
                    // Go back to # punctuator, so that preprocess can handle after this function returns.
                    context.cur_index -= 1;
                    break;
                } else {
                    if matches!(directive, "endif") {
                        context.exit_if();
                    }
                    context.eat_token();
                }
            } else if matches!(directive, "if" | "ifdef" | "ifndef") {
                context.enter_if();
                context.eat_token();
            }
        } else {
            context.eat_token();
        }
    }
    return &context.tokens[start_index..context.cur_index];
}

pub fn eat_remaining_cur_if_layer(context: &mut PP_Context) {
    debug_assert!(context.cur_if_layer() > 0);

    let start_span = context.cur_token().span;
   
    let start_index = context.cur_index;
    let target_if_layer = context.cur_if_layer();
    loop {
        if context.cur_token().kind == Eof {
            let error_description = "Can't find a matching #endif for this directive.";
            report_preprocess_error(context.cur_if_directive_position(), &error_description);
            break;
        };
        if context.cur_token().at_bol && context.cur_token().kind == Punct("#") {
            context.eat_token();
            let directive = get_name(&context.cur_token().kind);
            if matches!(directive, "endif") {
                context.eat_token();
                context.exit_if();
                if context.cur_if_layer() == target_if_layer - 1 {
                    // Skip the whole if group, including the ending "#endif".
                    break;
                }
            } else if matches!(directive, "if" | "ifdef" | "ifndef") {
                context.enter_if();
                context.eat_token();
            }
        } else {
            context.eat_token();
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

fn get_name<'a>(kind: &'a TokenKind) -> &'a str {
    match kind {
        LexIdent(name) => name.as_str(),
        Keyword(name) => name,
        _ => "",
    }
}