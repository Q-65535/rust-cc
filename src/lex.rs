use crate::common::*;
use crate::SRC;
use colored::*;
use std::process::exit;

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    Punct(&'static str),
    Keyword(&'static str),
    // @Refactor?: Should we just use u64 for integer constant all the way during compilation?
    Lex_Integer { value: i64, ty: Integer_Const_Type },
    Lex_Float(f32),
    Lex_Double(f64),
    Lex_Unsigned(u64),
    LexIdent(String),
    StringLiteral(Vec<u8>),

    Eof,
}
use TokenKind::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Integer_Const_Type {
    tInt,
    tLong,
    tUInt,
    tULong,
}
use Integer_Const_Type::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

const qweqr: &str = "swewreqw";

// const seqwq: [&str] = ["ss", "qwe"];

// Longest spellings come first so lexing follows C's maximal-munch rule.
const PUNCTUATORS: &[&str] = &[
    "<<=", ">>=", "...", "->", "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "^=", "|=", "&&",
    "||", "<<", ">>", "==", "!=", "<=", ">=", ".", "+", "-", "*", "/", "%", "&", "^", "|", "(",
    ")", "{", "}", "[", "]", "=", "<", ">", "!", "~", "?", ";", ":", ",",
];

const KEYWORDS: &[&str] = &[
    "return", "if", "else", "for", "while", "int", "sizeof", "_Alignof", "_Alignas",
    "typedef", "struct", "union", "enum", "char", "long", "short", "void", "float",
    "double", "_Atomic", "_Bool", "static", "extern", "goto", "break", "continue",
    "switch", "case", "default", "do", "signed", "unsigned", "const", "volatile", "auto",
    "register", "restrict", "_Noreturn",
];

pub struct Lexer<'a> {
    src_ref: &'a str,
    index: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            src_ref: source,
            index: 0,
        }
    }

    fn keyword_kind(name: &str) -> Option<TokenKind> {
        match name {
            "__restrict" | "__restrict__" => return Some(Keyword("restrict")),
            _ => {
                for k in KEYWORDS {
                    if name == *k {
                        return Some(Keyword(k));
                    }
                }
                return None;
            }
        }
    }

    fn cur_char(&self) -> char {
        self.char_at(self.index)
    }

    fn char_at(&self, index: usize) -> char {
        self.src_ref.as_bytes()[index] as char
    }

    fn has_next(&self) -> bool {
        let len = self.src_ref.len();
        self.index + 1 < len
    }

    fn next_char(&mut self) {
        debug_assert!(self.has_next());
        self.index += 1;
    }

    fn skip_cur_char(&mut self, c: char) {
        debug_assert!(self.cur_char() == c);
        self.next_char();
    }

    fn peek_char(&self) -> Option<char> {
        if self.index < self.src_ref.len() - 1 {
            Some(self.char_at(self.index + 1))
        } else {
            None
        }
    }

    pub fn gen_token(kind: TokenKind, start_index: usize, len: usize) -> Token {
        let span = Span {
            start_index,
            end_index: start_index + len - 1,
        };
        Token { kind, span }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        if self.src_ref.is_empty() {
            tokens.push(Self::gen_token(Eof, self.src_ref.len(), 1));
            return tokens;
        }
        loop {
            let c = self.cur_char();
            let start_index = self.index;
            match c {
                ' ' | '\t' | '\n' | '\r' => (),
                '.' if matches!(self.peek_char(), Some('0'..='9')) => {
                    let kind = self.read_num();
                    tokens.push(Self::gen_token(
                        kind,
                        start_index,
                        self.index - start_index + 1,
                    ));
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let name = self.read_ident();
                    let len = name.len();
                    let kind = Self::keyword_kind(&name).unwrap_or(LexIdent(name));
                    tokens.push(Self::gen_token(kind, start_index, len));
                }
                '0'..='9' => {
                    let kind = self.read_num();
                    tokens.push(Self::gen_token(
                        kind,
                        start_index,
                        self.index - start_index + 1,
                    ));
                }
                '\'' => {
                    let character = self.read_char_literal();
                    match character {
                        Ok(byte) => {
                            let end_index = self.index;
                            let len = end_index - start_index + 1;
                            // @Note: Both GCC and this compiler treat char literal as 32-bit signed integer.
                            // However, the difference is that GCC sign extended the char from 8-bit to 32-bit,
                            // while this compiler just 0 extend the char from 8-bit to 32-bit: we first 0 extend
                            // it to 64-bit using "byte as u64" below, then at analyze phase we add type Int to it
                            // which effectively means (later in codegen phase) to extract the lower 32-bit and
                            // interpret that lower 32-bit as signed integer and since 8-bit char literal fits in 32-bit
                            // integer, the extraction doesn't affect the char literal value, the result is a positive
                            // number whose data type is 32-bit singed integer.
                            // So, in GCC, (-128=='\x80') evaluates to 1, in this compiler, (128=='\x80') evaluates to 1.
                            // I don't known whether this difference will cause any problem, we'll see.
                            tokens.push(Self::gen_token(
                                Lex_Integer {
                                    value: byte as i64,
                                    ty: tInt,
                                },
                                start_index,
                                len,
                            ));
                        }
                        Err(s) => {
                            lexical_error_at(start_index, &s);
                        }
                    }
                }
                '"' => match self.read_string() {
                    Ok(bytes) => {
                        let consumed = self.index - start_index + 1;
                        tokens.push(Self::gen_token(StringLiteral(bytes), start_index, consumed));
                    }
                    Err(s) => {
                        lexical_error_at(start_index, &s);
                    }
                }
                '/' => {
                    match self.peek_char() {
                        Some('/') => {
                            // Line comment: skip until end of line.
                            self.next_char();
                            while let Some(nc) = self.peek_char() {
                                if nc == '\n' {
                                    break;
                                }
                                self.next_char();
                            }
                        }
                        Some('*') => {
                            // Block comment: skip until closing "*/".
                            self.next_char();
                            loop {
                                match self.peek_char() {
                                    Some('*') => {
                                        self.next_char();
                                        if let Some('/') = self.peek_char() {
                                            self.next_char();
                                            break;
                                        }
                                    }
                                    Some(_) => self.next_char(),
                                    None => {
                                        lexical_error_at(start_index, "unclosed block comment");
                                    }
                                }
                            }
                        }
                        _ => {
                            let punctuator = self.read_punctuator().unwrap();
                            tokens.push(Self::gen_token(
                                Punct(punctuator),
                                start_index,
                                punctuator.len(),
                            ));
                        }
                    }
                }
                _ => {
                    if let Some(punctuator) = self.read_punctuator() {
                        tokens.push(Self::gen_token(
                            Punct(punctuator),
                            start_index,
                            punctuator.len(),
                        ));
                    } else {
                        lexical_error_at(start_index, &format!("Unknown character: '{}'.", c));
                    }
                }
            }
            if self.has_next() {
                self.next_char();
            } else {
                break;
            }
        }
        // Rust interprets slicing indices as pointing to the spaces between elements,
        // not the elements themselves.
        // Since we set Eof token's start_index to src.len(), length to 0,
        // both start_index and end_index are src.len(), i.e., both
        // points to the space at the very end after the last element.
        // So We get an empty string from src[start_index.. end_index].
        // In fact, for any string and index:
        // as long as 0 ≤ index ≤ string.len() satisfied, string[index.. index] is a empty string.
        tokens.push(Self::gen_token(Eof, self.src_ref.len(), 0));
        tokens
    }

    fn read_punctuator(&mut self) -> Option<&'static str> {

        let rest = &self.src_ref[self.index..];
        for punct in PUNCTUATORS {
            if rest.starts_with(punct) {
                self.index += punct.len() - 1;
                return Some(punct);
            }
        }
        return None;
    }

    // This function can read integer and floating point constant number in C.
    // For floating point number this function return either Lex_Double(f64) or Lex_Float(f32).
    // For integer number, this function return Lex_Integer{}.
    fn read_num(&mut self) -> TokenKind {
        debug_assert!(matches!(self.cur_char(), '0'..='9' | '.'));

        let start = self.index;
        let len = self.src_ref.len();
        let mut end = start;

        if self.cur_char() == '0' && matches!(self.peek_char(), Some('x' | 'X')) {
            end += 2;

            let digits_start = end;
            while end < len && self.char_at(end).is_ascii_hexdigit() {
                end += 1;
            }
            let has_integer_digits = end > digits_start;

            let mut has_fraction_digits = false;
            let mut is_float = false;
            if end < len && self.char_at(end) == '.' {
                is_float = true;
                end += 1;
                let fraction_start = end;
                while end < len && self.char_at(end).is_ascii_hexdigit() {
                    end += 1;
                }
                has_fraction_digits = end > fraction_start;
            }

            if !has_integer_digits && !has_fraction_digits {
                lexical_error_at(start, "invalid hex number format");
            }

            if end < len && matches!(self.char_at(end), 'p' | 'P') {
                is_float = true;
                end += 1;
                if end < len && matches!(self.char_at(end), '+' | '-') {
                    end += 1;
                }

                let exponent_start = end;
                while end < len && self.char_at(end).is_ascii_digit() {
                    end += 1;
                }
                if end == exponent_start {
                    lexical_error_at(start, "invalid hex floating exponent");
                }
            } else if is_float {
                lexical_error_at(start, "hex floating constant requires a binary exponent");
            }

            if is_float {
                return self.finish_float(start, end, true);
            }

            return self.read_int();
        }

        while end < len && self.char_at(end).is_ascii_digit() {
            end += 1;
        }
        let mut is_float = self.cur_char() == '.';

        if end < len && self.char_at(end) == '.' {
            is_float = true;
            end += 1;
            while end < len && self.char_at(end).is_ascii_digit() {
                end += 1;
            }
        }

        if end < len && matches!(self.char_at(end), 'e' | 'E') {
            is_float = true;
            end += 1;
            if end < len && matches!(self.char_at(end), '+' | '-') {
                end += 1;
            }

            let exponent_start = end;
            while end < len && self.char_at(end).is_ascii_digit() {
                end += 1;
            }
            if end == exponent_start {
                lexical_error_at(start, "invalid floating exponent");
            }
        }

        if is_float {
            return self.finish_float(start, end, false);
        }

        self.read_int()
    }

    fn finish_float(&mut self, start: usize, mut end: usize, is_hex: bool) -> TokenKind {
        let mut is_float_type = false;
        if end < self.src_ref.len() {
            match self.char_at(end) {
                'f' | 'F' => {
                    is_float_type = true;
                    end += 1;
                }
                'l' | 'L' => {
                    end += 1;
                }
                c if Self::is_ident_continue(c) => {
                    lexical_error_at(end, "invalid suffix on floating constant");
                }
                _ => (),
            }
        }

        if end < self.src_ref.len() && Self::is_ident_continue(self.char_at(end)) {
            lexical_error_at(end, "invalid suffix on floating constant");
        }

        let number_end = if matches!(self.char_at(end - 1), 'f' | 'F' | 'l' | 'L') {
            end - 1
        } else {
            end
        };
        let literal = self.src_ref[start..number_end].to_string();
        self.index = end - 1;

        let value = if is_hex {
            Self::parse_hex_float_literal(&literal, start)
        } else {
            let normalized = Self::normalize_decimal_float_literal(&literal);
            match normalized.parse::<f64>() {
                Ok(value) => value,
                Err(_) => lexical_error_at(start, "invalid floating number format"),
            }
        };

        if is_float_type {
            Lex_Float(value as f32)
        } else {
            Lex_Double(value)
        }
    }

    fn normalize_decimal_float_literal(literal: &str) -> String {
        let mut normalized = literal.to_string();
        if normalized.starts_with('.') {
            normalized.insert(0, '0');
        }

        if let Some(exponent_index) = normalized.find('e').or_else(|| normalized.find('E')) {
            if normalized[..exponent_index].ends_with('.') {
                normalized.insert(exponent_index, '0');
            }
        } else if normalized.ends_with('.') {
            normalized.push('0');
        }

        normalized
    }

    fn parse_hex_float_literal(literal: &str, start: usize) -> f64 {
        let exponent_index = match literal.find('p').or_else(|| literal.find('P')) {
            Some(index) => index,
            None => lexical_error_at(start, "hex floating constant requires a binary exponent"),
        };
        let mantissa = &literal[2..exponent_index];
        let exponent = match literal[exponent_index + 1..].parse::<i32>() {
            Ok(exponent) => exponent,
            Err(_) => lexical_error_at(start, "invalid hex floating exponent"),
        };

        let mut value = 0.0;
        let mut fraction_scale = 1.0 / 16.0;
        let mut past_dot = false;
        for c in mantissa.chars() {
            if c == '.' {
                past_dot = true;
                continue;
            }

            let digit = match c.to_digit(16) {
                Some(digit) => digit as f64,
                None => lexical_error_at(start, "invalid hex floating number format"),
            };

            if past_dot {
                value += digit * fraction_scale;
                fraction_scale /= 16.0;
            } else {
                value = value * 16.0 + digit;
            }
        }

        value * 2.0f64.powi(exponent)
    }

    fn is_ident_continue(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn read_int(&mut self) -> TokenKind {
        debug_assert!(matches!(self.cur_char(), '0'..='9'));
        let c = self.cur_char();
        let mut base = 10;
        if c == '0' {
            base = 8;
            if self.peek_char() == Some('b') || self.peek_char() == Some('B') {
                base = 2;
                self.next_char();
                self.next_char();
            }
            if self.peek_char() == Some('x') || self.peek_char() == Some('X') {
                base = 16;
                self.next_char();
                self.next_char();
            }
        }

        let mut result: u64 = 0;
        if base == 10 {
            loop {
                let cur_digit = self.cur_char() as u64 - '0' as u64;
                result *= base;
                result += cur_digit;
                if !matches!(self.peek_char(), Some('0'..='9')) {
                    break;
                }
                self.next_char();
            }
        }
        if base == 8 {
            loop {
                let cur_digit = self.cur_char() as u64 - '0' as u64;
                result *= base;
                result += cur_digit;
                if !matches!(self.peek_char(), Some('0'..='7')) {
                    break;
                }
                self.next_char();
            }
        }
        if base == 2 {
            loop {
                let cur_digit = self.cur_char() as u64 - '0' as u64;
                result *= base;
                result += cur_digit;
                if !matches!(self.peek_char(), Some('0'..='1')) {
                    break;
                }
                self.next_char();
            }
        }
        if base == 16 {
            loop {
                let mut cur_digit: u64 = 0;
                if matches!(self.cur_char(), '0'..='9') {
                    cur_digit = self.cur_char() as u64 - '0' as u64;
                } else if matches!(self.cur_char(), 'a'..='f') {
                    cur_digit = self.cur_char() as u64 - 'a' as u64 + 10;
                } else if matches!(self.cur_char(), 'A'..='F') {
                    cur_digit = self.cur_char() as u64 - 'A' as u64 + 10;
                }
                result *= base;
                result += cur_digit;

                if !matches!(self.peek_char(), Some('0'..='9' | 'a'..='f' | 'A'..='F')) {
                    break;
                }
                self.next_char();
            }
        }

        let mut l_count = 0;
        let mut u_count = 0;
        while matches!(self.peek_char(), Some('l' | 'L' | 'u' | 'U')) {
            self.next_char();
            if matches!(self.cur_char(), 'l' | 'L') {
                l_count += 1;
            } else if matches!(self.cur_char(), 'u' | 'U') {
                u_count += 1;
            }
        }
        let mut l = false;
        let mut u = false;
        if l_count > 0 {
            if l_count > 2 {
                let err_msg = format!(
                    "At most 2 L (or l) suffix is allowed, but you give {} of it.",
                    l_count
                );
                lexical_error_at(self.index, &err_msg);
            } else {
                l = true;
            }
        }
        if u_count > 0 {
            if u_count > 1 {
                let err_msg = format!(
                    "At most 1 U (or u) suffix is allowed, but you give {} of it.",
                    u_count
                );
                lexical_error_at(self.index, &err_msg);
            } else {
                u = true;
            }
        }

        if let Some(c) = self.peek_char() {
            if c.is_ascii_alphanumeric() {
                self.next_char();
                let err_msg = format!("Invalid suffix for constant integer number.");
                lexical_error_at(self.index, &err_msg);
            }
        }

        // Infer a type.
        let ty: Integer_Const_Type;
        if base == 10 {
            if l && u {
                ty = tULong;
            } else if l {
                ty = tLong;
            } else if u {
                ty = if (result >> 32) != 0 { tULong } else { tUInt };
            } else {
                ty = if (result >> 31) != 0 { tLong } else { tInt };
            }
        } else {
            if l && u {
                ty = tULong;
            } else if l {
                ty = if (result >> 63) != 0 { tULong } else { tLong };
            } else if u {
                ty = if (result >> 32) != 0 { tULong } else { tInt };
            // According to C spec:
            // For unsuffixed non-decimal integer constants, choose the first
            // type whose range contains the value, in this order:
            // int -> unsigned int -> long -> unsigned long.
            } else if (result >> 63) != 0 {
                ty = tULong;
            } else if (result >> 32) != 0 {
                ty = tLong;
            } else if (result >> 31) != 0 {
                ty = tUInt;
            } else {
                ty = tInt;
            }
        }

        return Lex_Integer {
            value: result as i64,
            ty,
        };
    }

    // @Question: Should we return a u8 or i8?
    fn read_char_literal(&mut self) -> Result<u8, String> {
        debug_assert!(matches!(self.cur_char(), '\''));
        self.next_char();
        if self.cur_char() == '\0' {
            let error_info = "unclosed char literal".to_string();
            return Err(error_info);
        }
        let the_char_literal = if self.cur_char() == '\\' {
            self.read_escaped_char()
        } else {
            self.cur_char() as u8
        };
        self.next_char();
        if self.cur_char() != '\'' {
            let error_info = "unclosed char literal".to_string();
            return Err(error_info);
        }
        return Ok(the_char_literal);
    }

    fn read_string(&mut self) -> Result<Vec<u8>, String> {
        debug_assert!(matches!(self.cur_char(), '"'));
        let mut bytes: Vec<u8> = Vec::new();
        loop {
            if self.has_next() {
                self.next_char();
            } else {
                let error_info =
                    "reaching end of file without seeing closing \" while parsing string literal"
                        .to_string();
                return Err(error_info);
            }
            if self.cur_char() == '"' {
                return Ok(bytes);
            }
            if self.cur_char() == '\\' {
                bytes.push(self.read_escaped_char());
            } else {
                bytes.push(self.src_ref.as_bytes()[self.index]);
            }
        }
    }

    fn read_escaped_char(&mut self) -> u8 {
        self.skip_cur_char('\\');
        let c = self.cur_char();
        if c >= '0' && c <= '7' {
            return self.read_escaped_octal();
        } else if c == 'x' {
            return self.read_escaped_hex();
        }
        match c {
            'a' => 0x07,
            'b' => 0x08,
            't' => b'\t',
            'n' => b'\n',
            'v' => 0x0B,
            'f' => 0x0C,
            'r' => b'\r',
            'e' => 0x1B,
            _ => c as u8,
        }
    }

    fn read_escaped_octal(&mut self) -> u8 {
        let mut num: u8 = 0;
        // A char is at most 255 (8 bits), so a 3-digit octal number suffices.
        for _ in 0..3 {
            let digit_number = self.cur_char() as u8 - b'0';
            num = num.wrapping_shl(3).wrapping_add(digit_number);
            if self.peek_char() >= Some('0') && self.peek_char() <= Some('7') {
                self.next_char();
            } else {
                break;
            }
        }
        num
    }

    fn read_escaped_hex(&mut self) -> u8 {
        self.skip_cur_char('x');
        let mut num: u8 = 0;
        let mut c = self.cur_char();
        if !c.is_ascii_hexdigit() {
            let error_message = format!("lex hexdigit error, wrong character following \\x");
            lexical_error_at(self.index, &error_message);
        }
        while c.is_ascii_hexdigit() {
            let digit_number;
            if c >= '0' && c <= '9' {
                digit_number = c as u8 - b'0';
            } else if c >= 'A' && c <= 'F' {
                digit_number = c as u8 - b'A' + 10;
            } else {
                digit_number = c as u8 - b'a' + 10;
            }
            // Per the C standard, \x consumes an unbounded run of hex digits;
            // overflow past one byte is allowed and the low 8 bits are kept.
            num = num.wrapping_shl(4).wrapping_add(digit_number);
            if self
                .peek_char()
                .expect("no char left to be peeked")
                .is_ascii_hexdigit()
            {
                self.next_char();
                c = self.cur_char();
            } else {
                break;
            }
        }
        num
    }

    fn read_ident(&mut self) -> String {
        debug_assert!(matches!(self.cur_char(), 'A'..='Z' | 'a'..='z' | '_'));
        let mut len = 1;
        let i = self.index;
        loop {
            match self.peek_char() {
                Some(c) => {
                    if (c <= '9' && c >= '0')
                        | (c >= 'a' && c <= 'z')
                        | (c >= 'A' && c <= 'Z')
                        | (c == '_')
                    {
                        len += 1;
                        self.next_char();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
        self.src_ref[i..i + len].to_string()
    }
}

fn lexical_error_at(index: usize, err_msg: &str) -> ! {
    use crate::error_span;
    let span = Span {
        start_index: index,
        end_index: index,
    };
    let error_stage_info = "Lexical error: ".to_string();
    let error_result = error_span(span, &(error_stage_info + err_msg));
    println!("{}", error_result);
    // Lex error is strict, once encountered, we force the compilation to stop.
    exit(1);
}

