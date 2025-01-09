use logos::Logos;
use std::str::FromStr;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    // Atoms, variables, and numbers
    #[regex("[a-z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Atom(String),
    #[regex("[A-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Variable(String),
    #[regex("[0-9]+", callback = parse_int)]
    Integer(i64),
    #[regex("[0-9]+\\.[0-9]+", callback = parse_float)]
    Float(f64),
    // Quoted atoms and character literals
    #[regex("'([^'\\\\]|\\\\[\\\\'\"])*'", |lex| {
        let slice = lex.slice();
        // Remove surrounding quotes and handle escape sequences
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
    // String literals with expanded escape sequences
    #[regex(r#""([^"\\]|\\[\\'"ntr])*""#, |lex| {
        let slice = lex.slice();
        // Remove surrounding quotes and handle escape sequences
        slice[1..slice.len()-1].replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\'", "'")
            .replace("\\\"", "\"")
            .replace("\\\\", "\\")
    })]
    String(String),
    // Additional operators and punctuation
    #[token(":-")]
    Implies,
    #[token("-->")]
    DCGArrow,
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
    // Extended comparison operators
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

pub fn tokenize(input: &str) -> Vec<Token> {
    Token::lexer(input).filter_map(Result::ok).collect()
}

fn main() {
    let input = r#"
        % This is a comment
        write("Hello,\nWorld!").
        /* This is a
           multi-line comment */
        calculate(X, Y) :- X =:= Y + 2.
        char_code(0'a).
        quoted_atom('hello\'world').
        bitwise(X, Y) :- X /\ Y =:= 0.
    "#;
    let tokens = tokenize(input);
    println!("{:?}", tokens);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let input = "append([], List, List).";
        let tokens = tokenize(input);
        assert!(matches!(tokens[0], Token::Atom(_)));
        assert!(matches!(tokens[1], Token::LParen));
        assert!(matches!(tokens[2], Token::LBracket));
        assert!(matches!(tokens[3], Token::RBracket));
        assert!(matches!(tokens[4], Token::Comma));
        assert!(matches!(tokens[5], Token::Variable(_)));
    }

    #[test]
    fn test_string_literals() {
        let input = r#"write("Hello,\nWorld!")."#;
        let tokens = tokenize(input);
        if let Token::String(s) = &tokens[2] {
            assert_eq!(s, "Hello,\nWorld!");
        } else {
            panic!("Expected string token");
        }
    }

    #[test]
    fn test_numbers() {
        let input = "X is 42 + 3.14";
        let tokens = tokenize(input);
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
        let tokens = tokenize(input);
        assert!(matches!(tokens[0], Token::Atom(_)));
        assert!(matches!(tokens[1], Token::LParen));
    }

    #[test]
    fn test_character_codes() {
        let input = "0'a 0'\\n 0'\\t 0'\\\\ 0'\\' 0'\\\"";
        let tokens: Vec<_> = tokenize(input);
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
        let tokens: Vec<_> = tokenize(input);
        assert_eq!(tokens, vec![
            Token::QuotedAtom("hello".to_string()),
            Token::QuotedAtom("hello'world".to_string()),
            Token::QuotedAtom("escape\\sequence".to_string()),
        ]);
    }
}
