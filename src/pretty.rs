// pretty.rs — Human-readable output for lexer tokens and AST nodes
//
// Usage:
//   mod pretty;
//   pretty::print_tokens(&tokens);
//   pretty::print_program(&program);

use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::common::Span;
use crate::SRC;

// ─────────────────────────────────────────────
//  ANSI colours
// ─────────────────────────────────────────────
const RESET:   &str = "\x1b[0m";
const BOLD:    &str = "\x1b[1m";
const DIM:     &str = "\x1b[2m";
const CYAN:    &str = "\x1b[36m";
const YELLOW:  &str = "\x1b[33m";
const GREEN:   &str = "\x1b[32m";
const MAGENTA: &str = "\x1b[35m";
const RED:     &str = "\x1b[31m";
const BLUE:    &str = "\x1b[34m";

// ═══════════════════════════════════════════════════════
//  LEXER OUTPUT
// ═══════════════════════════════════════════════════════

pub fn print_tokens(tokens: &[Token]) {
    const TYPE_W:   usize = 18;
    const LEXEME_W: usize = 20;

    let top = format!(
        "┌{:─<tw$}┬{:─<lw$}┬{:─<8}┬{:─<8}┐",
        "", "", "", "",
        tw = TYPE_W + 2, lw = LEXEME_W + 2
    );
    let div = format!(
        "├{:─<tw$}┼{:─<lw$}┼{:─<8}┼{:─<8}┤",
        "", "", "", "",
        tw = TYPE_W + 2, lw = LEXEME_W + 2
    );
    let bot = format!(
        "└{:─<tw$}┴{:─<lw$}┴{:─<8}┴{:─<8}┘",
        "", "", "", "",
        tw = TYPE_W + 2, lw = LEXEME_W + 2
    );

    println!("{BOLD}── Tokens ──────────────────────────────────────────{RESET}");
    println!("{top}");
    println!(
        "│ {BOLD}{:<tw$}{RESET} │ {BOLD}{:<lw$}{RESET} │ {BOLD}{:<6}{RESET} │ {BOLD}{:<6}{RESET} │",
        "TYPE", "LEXEME", "LINE", "COL",
        tw = TYPE_W, lw = LEXEME_W
    );
    println!("{div}");

    for tok in tokens {
        let (kind_str, colour) = token_kind_label(&tok.kind);
        let src = crate::SRC.lock().unwrap();
        let lexeme = &src[tok.span.start_index.. tok.span.end_index+1];
        let line   = tok.span.get_start_line();
        let col    = tok.span.start_index;

        let lex_disp: String = lexeme.chars().take(LEXEME_W).collect();
        println!(
            "│ {colour}{:<tw$}{RESET} │ {DIM}{:<lw$}{RESET} │ {line:>6} │ {col:>6} │",
            kind_str, lex_disp,
            tw = TYPE_W, lw = LEXEME_W
        );
    }

    println!("{bot}");
}

fn token_kind_label(kind: &TokenKind) -> (&'static str, &'static str) {
    match kind {
        TokenKind::Ret               => ("Keyword",   BLUE),
        TokenKind::If                => ("Keyword",   BLUE),
        TokenKind::Else              => ("Keyword",   BLUE),
        TokenKind::For               => ("Keyword",   BLUE),
        TokenKind::While             => ("Keyword",   BLUE),
        TokenKind::Sizeof            => ("Keyword",   BLUE),
        TokenKind::Struct            => ("Keyword",   BLUE),
        TokenKind::Int               => ("Keyword",   BLUE),
        TokenKind::Char              => ("Keyword",   BLUE),
        TokenKind::LexIdent(_)       => ("Ident",     CYAN),
        TokenKind::Num(_)            => ("Number",    GREEN),
        TokenKind::StringLiteral(_)  => ("String",    GREEN),
        TokenKind::Plus              => ("Plus",      YELLOW),
        TokenKind::Minus             => ("Minus",     YELLOW),
        TokenKind::Mul               => ("Mul",       YELLOW),
        TokenKind::Div               => ("Div",       YELLOW),
        TokenKind::Ampersand         => ("Ampersand", YELLOW),
        TokenKind::Not               => ("Not",       YELLOW),
        TokenKind::Assignment        => ("Assign",    MAGENTA),
        TokenKind::Eq                => ("Compare",   MAGENTA),
        TokenKind::Neq               => ("Compare",   MAGENTA),
        TokenKind::LT                => ("Compare",   MAGENTA),
        TokenKind::LE                => ("Compare",   MAGENTA),
        TokenKind::GT                => ("Compare",   MAGENTA),
        TokenKind::GE                => ("Compare",   MAGENTA),
        TokenKind::LParen            => ("LParen",    DIM),
        TokenKind::RParen            => ("RParen",    DIM),
        TokenKind::LBrace            => ("LBrace",    DIM),
        TokenKind::RBrace            => ("RBrace",    DIM),
        TokenKind::LSqureBracket     => ("LBracket",  DIM),
        TokenKind::RSqureBracket     => ("RBracket",  DIM),
        TokenKind::Semicolon         => ("Semicolon", DIM),
        TokenKind::Comma             => ("Comma",     DIM),
        TokenKind::Period            => ("Period",    DIM),
        TokenKind::Arrow             => ("Arrow",     DIM),
        TokenKind::Eof               => ("EOF",       RED),
    }
}

// ═══════════════════════════════════════════════════════
//  AST OUTPUT
// ═══════════════════════════════════════════════════════

pub fn print_program(program: &Program) {
    println!("{BOLD}── AST ──────────────────────────────────────────────{RESET}");
    println!("{BOLD}{CYAN}Program{RESET}");
    let funs: Vec<_> = program.translation_units.iter().filter_map(|u| {
        if let TranslationUnit::FunctionDef(f) = u { Some(f) } else { None }
    }).collect();
    let n = funs.len();
    for (i, fun) in funs.iter().enumerate() {
        print_function(fun, "", i + 1 == n);
    }
    println!();
}

// ── Tree-drawing constants ────────────────────────────

const TEE:  &str = "├── ";
const LAST: &str = "└── ";
const PIPE: &str = "│   ";
const GAP:  &str = "    ";

fn child_prefix(prefix: &str, is_last: bool) -> String {
    format!("{}{}", prefix, if is_last { GAP } else { PIPE })
}

fn branch(prefix: &str, is_last: bool, label: &str) {
    let conn = if is_last { LAST } else { TEE };
    println!("{prefix}{conn}{label}");
}

// ── Function ──────────────────────────────────────────

fn print_function(fun: &Function, prefix: &str, is_last: bool) {
    let stars = "*".repeat(fun.star_count as usize);
    let ret   = decl_spec_str(&fun.return_type);
    let conn  = if is_last { LAST } else { TEE };
    println!(
        "{prefix}{conn}{BOLD}{GREEN}fn{RESET} {BOLD}{}{RESET}  {DIM}→ {stars}{ret}{RESET}",
        fun.name
    );

    let cp = child_prefix(prefix, is_last);
    let has_body = !fun.items.is_empty();

    // params
    let param_conn = if has_body { TEE } else { LAST };
    println!("{cp}{param_conn}{BLUE}params{RESET}");
    let pp = child_prefix(&cp, !has_body);
    if fun.params.is_empty() {
        println!("{pp}{LAST}{DIM}(none){RESET}");
    } else {
        let pc = fun.params.len();
        for (i, p) in fun.params.iter().enumerate() {
            print_parameter(p, &pp, i + 1 == pc);
        }
    }

    // body
    if has_body {
        println!("{cp}{LAST}{BLUE}body{RESET}");
        let bp = child_prefix(&cp, true);
        let bc = fun.items.len();
        for (i, item) in fun.items.iter().enumerate() {
            print_block_item(item, &bp, i + 1 == bc);
        }
    }
}

fn print_parameter(p: &Parameter, prefix: &str, is_last: bool) {
    let stars = "*".repeat(p.declarator.star_count as usize);
    let ty    = decl_spec_str(&p.decl_spec);
    branch(
        prefix, is_last,
        &format!("{CYAN}param{RESET} {stars}{ty} {}", p.declarator.name),
    );
}

// ── Block items ───────────────────────────────────────

fn print_block_item(item: &BlockItem, prefix: &str, is_last: bool) {
    match item {
        BlockItem::Stmt(s) => print_stmt(s, prefix, is_last),
        BlockItem::Decl(d) => print_declaration(d, prefix, is_last),
    }
}

fn print_declaration(d: &Declaration, prefix: &str, is_last: bool) {
    let ty   = decl_spec_str(&d.decl_spec);
    let conn = if is_last { LAST } else { TEE };
    println!("{prefix}{conn}{MAGENTA}Decl{RESET}  {DIM}type={ty}{RESET}");
    let cp = child_prefix(prefix, is_last);
    let ic = d.declarators.len();
    for (i, id) in d.declarators.iter().enumerate() {
        print_declarator(id, ty, &cp, i + 1 == ic);
    }
}

fn print_declarator(id: &Declarator, base_ty: &str, prefix: &str, is_last: bool) {
    let stars  = "*".repeat(id.star_count as usize);
    let suffix = declarator_suffix_str(&id.suffix);
    let conn   = if is_last { LAST } else { TEE };
    println!(
        "{prefix}{conn}{CYAN}{}{RESET}{DIM}: {stars}{base_ty}{suffix}{RESET}",
        id.name
    );
    if let Some(expr) = &id.init_expr {
        let cp = child_prefix(prefix, is_last);
        print_expr(expr, &cp, true);
    }
}

// ── Statements ────────────────────────────────────────

fn print_stmt(s: &StmtType, prefix: &str, is_last: bool) {
    let conn = if is_last { LAST } else { TEE };
    match s {
        StmtType::Ex(expr) => {
            println!("{prefix}{conn}{YELLOW}ExprStmt{RESET}");
            print_expr(expr, &child_prefix(prefix, is_last), true);
        }
        StmtType::Return(expr) => {
            println!("{prefix}{conn}{YELLOW}Return{RESET}");
            print_expr(expr, &child_prefix(prefix, is_last), true);
        }
        StmtType::Block(items) => {
            println!("{prefix}{conn}{YELLOW}Block{RESET}");
            let cp = child_prefix(prefix, is_last);
            let ic = items.len();
            for (i, item) in items.iter().enumerate() {
                print_block_item(item, &cp, i + 1 == ic);
            }
        }
        StmtType::If(IfStmt { cond, then, otherwise }) => {
            println!("{prefix}{conn}{YELLOW}If{RESET}");
            let cp = child_prefix(prefix, is_last);

            println!("{cp}{TEE}{DIM}cond{RESET}");
            print_expr(cond, &child_prefix(&cp, false), true);

            match otherwise {
                Some(else_branch) => {
                    println!("{cp}{TEE}{DIM}then{RESET}");
                    print_stmt(then, &child_prefix(&cp, false), true);
                    println!("{cp}{LAST}{DIM}else{RESET}");
                    print_stmt(else_branch, &child_prefix(&cp, true), true);
                }
                None => {
                    println!("{cp}{LAST}{DIM}then{RESET}");
                    print_stmt(then, &child_prefix(&cp, true), true);
                }
            }
        }
        StmtType::For(ForStmt { init, cond, inc, then }) => {
            println!("{prefix}{conn}{YELLOW}For{RESET}");
            let cp = child_prefix(prefix, is_last);
            print_opt_expr(&cp, "init", init, false);
            print_opt_expr(&cp, "cond", cond, false);
            print_opt_expr(&cp, "inc",  inc,  false);
            println!("{cp}{LAST}{DIM}body{RESET}");
            print_stmt(then, &child_prefix(&cp, true), true);
        }
    }
}

fn print_opt_expr(prefix: &str, label: &str, expr: &Option<Expr>, is_last: bool) {
    let conn = if is_last { LAST } else { TEE };
    match expr {
        Some(e) => {
            println!("{prefix}{conn}{DIM}{label}{RESET}");
            print_expr(e, &child_prefix(prefix, is_last), true);
        }
        None => {
            println!("{prefix}{conn}{DIM}{label}: (none){RESET}");
        }
    }
}

// ── Expressions ───────────────────────────────────────

fn print_expr(expr: &Expr, prefix: &str, is_last: bool) {
    let conn = if is_last { LAST } else { TEE };
    match &expr.content {
        ExprType::Number(n) => {
            println!("{prefix}{conn}{GREEN}Num{RESET}({n})");
        }
        ExprType::Ident(s) => {
            println!("{prefix}{conn}{CYAN}Ident{RESET}({s})");
        }
        ExprType::Binary(lhs, rhs, op) => {
            println!("{prefix}{conn}{YELLOW}Binary{RESET}  {DIM}{}{RESET}", token_op_str(op));
            let cp = child_prefix(prefix, is_last);
            print_expr(lhs, &cp, false);
            print_expr(rhs, &cp, true);
        }
        ExprType::Assign(lhs, rhs) => {
            println!("{prefix}{conn}{YELLOW}Assign{RESET}");
            let cp = child_prefix(prefix, is_last);
            print_expr(lhs, &cp, false);
            print_expr(rhs, &cp, true);
        }
        ExprType::Neg(inner) => {
            println!("{prefix}{conn}{YELLOW}Neg{RESET}");
            print_expr(inner, &child_prefix(prefix, is_last), true);
        }
        ExprType::Deref(inner) => {
            println!("{prefix}{conn}{YELLOW}Deref{RESET}");
            print_expr(inner, &child_prefix(prefix, is_last), true);
        }
        ExprType::AddrOf(inner) => {
            println!("{prefix}{conn}{YELLOW}AddrOf{RESET}");
            print_expr(inner, &child_prefix(prefix, is_last), true);
        }
        ExprType::Sizeof(inner) => {
            println!("{prefix}{conn}{YELLOW}Sizeof{RESET}");
            print_expr(inner, &child_prefix(prefix, is_last), true);
        }
        ExprType::ArrayIndexing(base, indices) => {
            println!("{prefix}{conn}{YELLOW}ArrayIndex{RESET}");
            let cp = child_prefix(prefix, is_last);
            let ic = indices.len();
            print_expr(base, &cp, ic == 0);
            for (i, idx) in indices.iter().enumerate() {
                print_expr(idx, &cp, i + 1 == ic);
            }
        }
        ExprType::Paren(inner) => {
            println!("{prefix}{conn}{YELLOW}Paren{RESET}");
            print_expr(inner, &child_prefix(prefix, is_last), true);
        }
        ExprType::CommaExpression(lhs, rhs) => {
            println!("{prefix}{conn}{YELLOW}Comma{RESET}");
            let cp = child_prefix(prefix, is_last);
            print_expr(lhs, &cp, false);
            print_expr(rhs, &cp, true);
        }
        ExprType::Str(bytes) => {
            let s = String::from_utf8_lossy(bytes);
            println!("{prefix}{conn}{GREEN}Str{RESET}({DIM}\"{}\"{RESET})", s.escape_debug());
        }
        ExprType::StmtExpr(items) => {
            println!("{prefix}{conn}{YELLOW}StmtExpr{RESET}");
            let cp = child_prefix(prefix, is_last);
            let ic = items.len();
            for (i, item) in items.iter().enumerate() {
                print_block_item(item, &cp, i + 1 == ic);
            }
        }
        ExprType::RequestStructMember(base, member) => {
            println!("{prefix}{conn}{YELLOW}Member{RESET}  {DIM}.{member}{RESET}");
            print_expr(base, &child_prefix(prefix, is_last), true);
        }
        ExprType::FunCall(callee, args) => {
            println!("{prefix}{conn}{YELLOW}FunCall{RESET}");
            let cp = child_prefix(prefix, is_last);
            let has_args = !args.is_empty();
            print_expr(callee, &cp, !has_args);
            if has_args {
                println!("{cp}{LAST}{DIM}args{RESET}");
                let ap = child_prefix(&cp, true);
                let ac = args.len();
                for (i, arg) in args.iter().enumerate() {
                    print_expr(arg, &ap, i + 1 == ac);
                }
            }
        }
    }
}

// ── Helpers ───────────────────────────────────────────

fn decl_spec_str(ds: &TypeSpec) -> &'static str {
    match ds {
        TypeSpec::Int => "int",
        TypeSpec::Char => "char",
        TypeSpec::Struct(_) => "struct",
    }
}

fn declarator_suffix_str(suffix: &Option<DeclaratorSuffix>) -> String {
    match suffix {
        None => String::new(),
        Some(DeclaratorSuffix::ArrayLen(dims)) => {
            dims.iter().map(|d| format!("[{d}]")).collect()
        }
        Some(DeclaratorSuffix::FunParam(params)) => {
            let ps: Vec<String> = params.iter().map(|p| {
                let stars = "*".repeat(p.declarator.star_count as usize);
                let ty    = decl_spec_str(&p.decl_spec);
                format!("{stars}{ty} {}", p.declarator.name)
            }).collect();
            format!("({})", ps.join(", "))
        }
    }
}

fn token_op_str(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::Plus       => "+",
        TokenKind::Minus      => "-",
        TokenKind::Mul        => "*",
        TokenKind::Div        => "/",
        TokenKind::Ampersand  => "&",
        TokenKind::Not        => "!",
        TokenKind::Eq         => "==",
        TokenKind::Neq        => "!=",
        TokenKind::LT         => "<",
        TokenKind::LE         => "<=",
        TokenKind::GT         => ">",
        TokenKind::GE         => ">=",
        _ => "?",
    }
}