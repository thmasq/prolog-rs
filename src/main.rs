use logos::Logos;
use std::ops::Range;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub token: T,
    pub span: Range<usize>,
}

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    // Atoms, variables, and numbers with Unicode support
    #[regex("[\\p{Lu}_][\\p{L}0-9_\u{0080}-\u{10FFFF}]*", priority = 2, callback = |lex| lex.slice().to_string())]
    Variable(String),
    #[regex("[a-z\u{0080}-\u{10FFFF}][a-zA-Z0-9_\u{0080}-\u{10FFFF}]*", priority = 1, callback = |lex| lex.slice().to_string())]
    Atom(String),

    // Extended number support
    #[regex("[0-9]+", callback = parse_int)]
    Integer(i64),
    #[regex("0[bB][01]+", callback = parse_binary)]
    #[regex("0[oO][0-7]+", callback = parse_octal)]
    #[regex("0[xX][0-9a-fA-F]+", callback = parse_hex)]
    NumericLiteral(i64),
    #[regex("[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?", callback = parse_float)]
    Float(f64),

    // Quoted atoms and character literals
    #[regex("'([^'\\\\]|\\\\[\\\\'\"])*'", |lex| {
        let slice = lex.slice();
        slice[1..slice.len()-1].replace("\\'", "'")
            .replace("\\\"", "\"")
            .replace("\\\\", "\\")
    })]
    QuotedAtom(String),
    #[regex("0'.", |lex| lex.slice()[2..].chars().next().unwrap() as u32)]
    #[regex("0'\\\\[ntr\"'\\\\]", |lex| {
        match &lex.slice()[2..] {
            "\\n" => '\n' as u32,
            "\\t" => '\t' as u32,
            "\\r" => '\r' as u32,
            "\\\"" => '"' as u32,
            "\\'" => '\'' as u32,
            "\\\\" => '\\' as u32,
            _ => 0
        }
    })]
    CharCode(u32),

    // String literals
    #[regex(r#""([^"\\]|\\[\\'"ntr])*""#, |lex| {
        let slice = lex.slice();
        slice[1..slice.len()-1].replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\'", "'")
            .replace("\\\"", "\"")
            .replace("\\\\", "\\")
    })]
    String(String),

    // Control flow operators
    #[token(":-")]
    Implies,
    #[token("-->")]
    DCGArrow,
    #[token("->")]
    IfThen,
    #[token("*->")]
    SoftCut,
    #[token("?-")]
    Query,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("!")]
    Cut,

    // Brackets and structural tokens
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("|")]
    Pipe,
    #[token("[]")]
    EmptyList,

    // Extended arithmetic operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("//")]
    IntegerDivide,
    #[token("mod")]
    Modulo,
    #[token("rem")]
    Remainder,
    #[token("**")]
    Power,
    #[token("<<")]
    BitShiftLeft,
    #[token(">>")]
    BitShiftRight,
    #[token("/\\")]
    BitAnd,
    #[token("\\/")]
    BitOr,
    #[token("\\")]
    BitNot,
    #[token("xor")]
    BitXor,

    // Comparison operators
    #[token("=")]
    Equals,
    #[token("\\=")]
    NotEquals,
    #[token("<")]
    LessThan,
    #[token("=<")]
    LessEquals,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterEquals,
    #[token("@<")]
    TermLessThan,
    #[token("@=<")]
    TermLessEquals,
    #[token("@>")]
    TermGreaterThan,
    #[token("@>=")]
    TermGreaterEquals,
    #[token("==")]
    TermEquals,
    #[token("\\==")]
    TermNotEquals,

    // Extended unification and term operators
    #[token("=:=")]
    ArithEquals,
    #[token("=\\=")]
    ArithNotEquals,
    #[token("is")]
    Is,
    #[token("=..")]
    Univ,
    #[token("^")]
    Caret,

    // Operator declaration
    #[token("op")]
    Op,
    #[token("fx")]
    Fx,
    #[token("fy")]
    Fy,
    #[token("xf")]
    Xf,
    #[token("yf")]
    Yf,
    #[token("xfx")]
    Xfx,
    #[token("xfy")]
    Xfy,
    #[token("yfx")]
    Yfx,

    // Skip whitespace and comments
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"%[^\n]*", logos::skip)]
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]
    Error,
}

fn parse_int(lex: &mut logos::Lexer<Token>) -> Option<i64> {
    i64::from_str(lex.slice()).ok()
}

fn parse_float(lex: &mut logos::Lexer<Token>) -> Option<f64> {
    f64::from_str(lex.slice()).ok()
}

fn parse_binary(lex: &mut logos::Lexer<Token>) -> Option<i64> {
    let slice = lex.slice();
    i64::from_str_radix(&slice[2..], 2).ok()
}

fn parse_octal(lex: &mut logos::Lexer<Token>) -> Option<i64> {
    let slice = lex.slice();
    i64::from_str_radix(&slice[2..], 8).ok()
}

fn parse_hex(lex: &mut logos::Lexer<Token>) -> Option<i64> {
    let slice = lex.slice();
    i64::from_str_radix(&slice[2..], 16).ok()
}

pub fn tokenize(input: &str) -> Vec<Spanned<Token>> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        if let Ok(token) = token {
            tokens.push(Spanned {
                token,
                span: lexer.span(),
            });
        }
    }
    tokens
}

fn main() {
    let input = r#"
        % Unicode and special numbers
        sum(Σ, 0xFF) :- 
            binary(0b1010),
            octal(0o755).
        
        % Operator declaration
        :- op(500, xfy, '⊕').
        
        % Control flow with if-then
        max(X, Y, Max) :-
            X > Y -> Max = X
            ;
            Max = Y.
        
        % List processing with |
        process([H|T]) :- 
            handle(H),
            process(T).
        process([]) :- !.
    "#;

    let tokens = tokenize(input);
    for Spanned { token, span } in tokens {
        println!("{:?} at position {:?}", token, span);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokens_only(tokens: Vec<Spanned<Token>>) -> Vec<Token> {
        tokens.into_iter().map(|s| s.token).collect()
    }

    #[test]
    fn test_basic_tokens() {
        let input = "append([], List, List).";
        let tokens = tokens_only(tokenize(input));
        assert!(matches!(tokens[0], Token::Atom(_)));
        assert!(matches!(tokens[1], Token::LParen));
        assert!(matches!(tokens[2], Token::EmptyList));
        assert!(matches!(tokens[3], Token::Comma));
        assert!(matches!(tokens[4], Token::Variable(_)));
    }

    #[test]
    fn test_string_literals() {
        let input = r#"write("Hello,\nWorld!")."#;
        let tokens = tokens_only(tokenize(input));
        if let Token::String(s) = &tokens[2] {
            assert_eq!(s, "Hello,\nWorld!");
        } else {
            panic!("Expected string token");
        }
    }

    #[test]
    fn test_numbers() {
        let input = "X is 42 + 3.14";
        let tokens = tokens_only(tokenize(input));
        assert!(matches!(tokens[0], Token::Variable(_)));
        assert!(matches!(tokens[1], Token::Is));
        assert!(matches!(tokens[2], Token::Integer(42)));
        assert!(matches!(tokens[3], Token::Plus));
        assert!(matches!(tokens[4], Token::Float(3.14)));
    }

    #[test]
    fn test_comments() {
        let input = "
            % Single line comment
            /* Multi-line
               comment */
            test(X).
        ";
        let tokens = tokens_only(tokenize(input));
        assert!(matches!(tokens[0], Token::Atom(_)));
        assert!(matches!(tokens[1], Token::LParen));
    }

    #[test]
    fn test_character_codes() {
        let input = "0'a 0'\\n 0'\\t 0'\\\\ 0'\\' 0'\\\"";
        let tokens = tokens_only(tokenize(input));
        assert_eq!(tokens, vec![
            Token::CharCode('a' as u32),
            Token::CharCode('\n' as u32),
            Token::CharCode('\t' as u32),
            Token::CharCode('\\' as u32),
            Token::CharCode('\'' as u32),
            Token::CharCode('"' as u32),
        ]);
    }

    #[test]
    fn test_quoted_atoms() {
        let input = "'hello' 'hello\\\'world' 'escape\\\\sequence'";
        let tokens = tokens_only(tokenize(input));
        assert_eq!(tokens, vec![
            Token::QuotedAtom("hello".to_string()),
            Token::QuotedAtom("hello'world".to_string()),
            Token::QuotedAtom("escape\\sequence".to_string()),
        ]);
    }

    #[test]
    fn test_numeric_literals() {
        let input = "42 0xFF 0b1010 0o755 3.14e-10";
        let tokens = tokens_only(tokenize(input));
        assert_eq!(tokens, vec![
            Token::Integer(42),
            Token::NumericLiteral(255), // 0xFF
            Token::NumericLiteral(10),  // 0b1010
            Token::NumericLiteral(493), // 0o755
            Token::Float(3.14e-10),
        ]);
    }

    #[test]
    fn test_unicode_identifiers() {
        let input = "测试 Δx αβγ";
        let tokens = tokens_only(tokenize(input));
        assert_eq!(tokens, vec![
            Token::Atom("测试".to_string()),
            Token::Variable("Δx".to_string()),
            Token::Atom("αβγ".to_string()),
        ]);
    }

    #[test]
    fn test_operator_declarations() {
        let input = ":- op(500, xfy, '+').";
        let tokens = tokens_only(tokenize(input));
        assert_eq!(tokens, vec![
            Token::Implies,
            Token::Op,
            Token::LParen,
            Token::Integer(500),
            Token::Comma,
            Token::Xfy,
            Token::Comma,
            Token::QuotedAtom("+".to_string()),
            Token::RParen,
            Token::Period,
        ]);
    }

    #[test]
    fn test_list_syntax() {
        let input = "[1,2|Rest]";
        let tokens = tokens_only(tokenize(input));
        assert_eq!(tokens, vec![
            Token::LBracket,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::Pipe,
            Token::Variable("Rest".to_string()),
            Token::RBracket,
        ]);
    }

    #[test]
    fn test_control_flow() {
        let input = "test :- condition -> action1 ; action2.";
        let tokens = tokens_only(tokenize(input));
        assert!(tokens.contains(&Token::IfThen));
        assert!(tokens.contains(&Token::Semicolon));
    }

    // New test to verify position tracking
    #[test]
    fn test_token_positions() {
        let input = "hello(World)";
        let tokens = tokenize(input);

        assert_eq!(tokens[0].span, 0..5); // "hello"
        assert_eq!(tokens[1].span, 5..6); // "("
        assert_eq!(tokens[2].span, 6..11); // "World"
        assert_eq!(tokens[3].span, 11..12); // ")"
    }
}
