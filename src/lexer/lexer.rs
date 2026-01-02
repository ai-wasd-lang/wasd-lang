//! Lexer implementation for WASD source code.

use super::token::{Span, Token};

/// The WASD lexer, which tokenizes source code.
pub struct Lexer<'a> {
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    indent_stack: Vec<usize>,
    pending_tokens: Vec<(Token, Span)>,
    at_line_start: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code.
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            indent_stack: vec![0],
            pending_tokens: Vec::new(),
            at_line_start: true,
        }
    }

    /// Tokenize the entire source and return all tokens.
    pub fn tokenize(&mut self) -> Vec<(Token, Span)> {
        let mut tokens = Vec::new();
        loop {
            let (token, span) = self.next_token();
            let is_eof = matches!(token, Token::Eof);
            tokens.push((token, span));
            if is_eof {
                break;
            }
        }
        tokens
    }

    /// Get the next token from the source.
    pub fn next_token(&mut self) -> (Token, Span) {
        // Return pending tokens first (for dedents)
        if let Some(token) = self.pending_tokens.pop() {
            return token;
        }

        // Handle indentation at line start
        if self.at_line_start {
            self.at_line_start = false;
            if let Some(token) = self.handle_indentation() {
                return token;
            }
        }

        self.skip_whitespace();

        let start = self.current_pos();

        let Some((_, ch)) = self.chars.next() else {
            // Emit remaining dedents at EOF
            while self.indent_stack.len() > 1 {
                self.indent_stack.pop();
                self.pending_tokens
                    .push((Token::Dedent, Span::new(start, start)));
            }
            if let Some(token) = self.pending_tokens.pop() {
                return token;
            }
            return (Token::Eof, Span::new(start, start));
        };

        let token = match ch {
            '\n' => {
                self.at_line_start = true;
                Token::Newline
            }

            // Single-char tokens
            '+' => Token::Plus,
            '*' => Token::Star,
            '%' => Token::Percent,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            '.' => {
                if self.peek_char() == Some('.') {
                    self.chars.next();
                    Token::DotDot
                } else {
                    Token::Dot
                }
            }
            '?' => Token::Question,
            '&' => Token::Ampersand,
            '|' => Token::Pipe,

            // Multi-char tokens
            '-' => {
                if self.peek_char() == Some('>') {
                    self.chars.next();
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            '=' => {
                if self.peek_char() == Some('>') {
                    self.chars.next();
                    Token::FatArrow
                } else if self.peek_char() == Some('=') {
                    self.chars.next();
                    Token::EqEq
                } else {
                    Token::Eq
                }
            }
            '!' => {
                if self.peek_char() == Some('=') {
                    self.chars.next();
                    Token::NotEq
                } else {
                    Token::Not
                }
            }
            '<' => {
                if self.peek_char() == Some('=') {
                    self.chars.next();
                    Token::LtEq
                } else {
                    Token::Lt
                }
            }
            '>' => {
                if self.peek_char() == Some('=') {
                    self.chars.next();
                    Token::GtEq
                } else {
                    Token::Gt
                }
            }
            ':' => {
                if self.peek_char() == Some(':') {
                    self.chars.next();
                    Token::DoubleColon
                } else {
                    Token::Colon
                }
            }
            '/' => {
                if self.peek_char() == Some('/') {
                    // Line comment
                    self.skip_line_comment();
                    return self.next_token();
                } else {
                    Token::Slash
                }
            }

            // String literals
            '"' => self.lex_string(),

            // Numbers
            c if c.is_ascii_digit() => self.lex_number(c),

            // Identifiers and keywords
            c if c.is_alphabetic() || c == '_' => self.lex_identifier(c),

            _ => panic!("Unexpected character: {}", ch),
        };

        let end = self.current_pos();
        (token, Span::new(start, end))
    }

    fn current_pos(&mut self) -> usize {
        self.chars
            .peek()
            .map(|(i, _)| *i)
            .unwrap_or(self.source.len())
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn skip_whitespace(&mut self) {
        while let Some(&(_, ch)) = self.chars.peek() {
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(&(_, ch)) = self.chars.peek() {
            if ch == '\n' {
                break;
            }
            self.chars.next();
        }
    }

    fn handle_indentation(&mut self) -> Option<(Token, Span)> {
        let start = self.current_pos();
        let mut indent = 0;

        while let Some(&(_, ch)) = self.chars.peek() {
            match ch {
                ' ' => {
                    indent += 1;
                    self.chars.next();
                }
                '\t' => {
                    indent += 4; // Treat tabs as 4 spaces
                    self.chars.next();
                }
                '\n' => {
                    // Empty line, skip
                    self.chars.next();
                    indent = 0;
                }
                _ => break,
            }
        }

        let current_indent = *self.indent_stack.last().unwrap();

        if indent > current_indent {
            self.indent_stack.push(indent);
            Some((Token::Indent, Span::new(start, self.current_pos())))
        } else if indent < current_indent {
            while self.indent_stack.len() > 1 && *self.indent_stack.last().unwrap() > indent {
                self.indent_stack.pop();
                self.pending_tokens
                    .push((Token::Dedent, Span::new(start, start)));
            }
            self.pending_tokens.pop()
        } else {
            None
        }
    }

    fn lex_string(&mut self) -> Token {
        let mut value = String::new();
        while let Some((_, ch)) = self.chars.next() {
            match ch {
                '"' => break,
                '\\' => {
                    if let Some((_, escaped)) = self.chars.next() {
                        match escaped {
                            'n' => value.push('\n'),
                            't' => value.push('\t'),
                            'r' => value.push('\r'),
                            '\\' => value.push('\\'),
                            '"' => value.push('"'),
                            _ => value.push(escaped),
                        }
                    }
                }
                _ => value.push(ch),
            }
        }
        Token::String(value)
    }

    fn lex_number(&mut self, first: char) -> Token {
        let mut value = String::from(first);
        let mut is_float = false;

        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_ascii_digit() {
                value.push(ch);
                self.chars.next();
            } else if ch == '.' && !is_float {
                // Peek ahead to check if this is a range operator (..)
                // We need to clone the iterator to peek further
                let mut peek_iter = self.chars.clone();
                peek_iter.next(); // consume the first .
                if let Some(&(_, next_ch)) = peek_iter.peek() {
                    if next_ch == '.' {
                        // This is a range operator, don't consume the .
                        break;
                    }
                }
                is_float = true;
                value.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        if is_float {
            Token::Float(value.parse().unwrap())
        } else {
            Token::Int(value.parse().unwrap())
        }
    }

    fn lex_identifier(&mut self, first: char) -> Token {
        let mut value = String::from(first);

        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                value.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        match value.as_str() {
            "let" => Token::Let,
            "mut" => Token::Mut,
            "fn" => Token::Fn,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "trait" => Token::Trait,
            "impl" => Token::Impl,
            "self" => Token::SelfKeyword,
            "match" => Token::Match,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "in" => Token::In,
            "return" => Token::Return,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "loop" => Token::Loop,
            "with" => Token::With,
            "use" => Token::Use,
            "import" => Token::Import,
            "pub" => Token::Pub,
            "extern" => Token::Extern,
            "as" => Token::As,
            "heap" => Token::Heap,
            "rc" => Token::Rc,
            "arc" => Token::Arc,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
            _ => Token::Ident(value),
        }
    }
}
