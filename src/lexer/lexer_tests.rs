//! Tests for the WASD lexer.

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::Token;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("let x = 5");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::Eq,
                Token::Int(5),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_indentation() {
        let source = "fn foo()\n    let x = 1\n    x";
        let mut lexer = Lexer::new(source);
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|(t, _)| t).collect();
        assert!(tokens.contains(&Token::Indent));
    }
}
