use std::path::PathBuf;

use crate::token::{Token, TokenKind};

pub struct Lexer {
    pub file_name: String,
    pub content: String,
    position: usize,
}

impl Lexer {
    pub fn new(path: PathBuf) -> Self {
        Self {
            content: std::fs::read_to_string(&path).unwrap(),
            position: 0,
            file_name: path.to_string_lossy().to_string(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.peek_n(0)
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        self.content[self.position..].chars().skip(n).next()
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.peek();
        if next.is_some() {
            self.position += 1;
        }

        next
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self, current: char) -> Token {
        let start = self.position - 1;
        let mut string = String::from(current);

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                string.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Token::new((start..self.position).into(), TokenKind::Number(string))
    }

    fn read_identifier(&mut self, current: char) -> Token {
        let start = self.position - 1;
        let mut string = String::from(current);

        while let Some(c) = self.peek() {
            if c.is_alphabetic() || c.is_numeric() || c == '$' {
                string.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Keywords
        let kind = match string.as_str() {
            "let" => TokenKind::Let,
            "ret" => TokenKind::Ret,
            "proc" => TokenKind::Proc,
            "extern" => TokenKind::Extern,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier(string),
        };

        Token::new((start..self.position).into(), kind)
    }

    fn read_string(&mut self) -> Token {
        let start = self.position - 1;
        let mut string = String::new();
        while let Some(c) = self.advance() {
            if c == '"' {
                break;
            }

            string.push(c);
        }

        Token::new((start..self.position).into(), TokenKind::String(string))
    }

    fn char(&self, kind: TokenKind) -> Token {
        Token::new((self.position - 1..self.position).into(), kind)
    }

    fn skip_to_newline(&mut self) {
        while let Some(c) = self.peek() {
            if c != '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn slash_or_comment(&mut self) -> Option<Token> {
        let start = self.position - 1;

        match self.peek()? {
            '/' => {
                self.skip_to_newline();
                self.next()
            }
            _ => Some(self.char(TokenKind::Slash)),
        }
    }

    fn dot_or_dotdotdot(&mut self) -> Option<Token> {
        let start = self.position - 1;
        let next = self.peek();
        let after_that = self.peek_n(1);

        if next.is_some() && after_that.is_some() {
            if next.unwrap() == '.' && after_that.unwrap() == '.' {
                self.advance();
                self.advance();

                return Some(Token::new(
                    (start..self.position).into(),
                    TokenKind::DotDotDot,
                ));
            }
        }

        return Some(self.char(TokenKind::Dot));
    }

    fn equal_or_compare(&mut self) -> Option<Token> {
        let start = self.position - 1;

        if let Some('=') = self.peek() {
            self.advance();
            Some(Token::new(
                (start..self.position).into(),
                TokenKind::EqualEqual,
            ))
        } else {
            Some(self.char(TokenKind::Equal))
        }
    }

    fn read_exclamation_equal(&mut self) -> Option<Token> {
        let start = self.position - 1;

        if let Some('=') = self.peek() {
            self.advance();
            Some(Token::new(
                (start..self.position).into(),
                TokenKind::ExclamationEqual,
            ))
        } else {
            // TODO: Find a way to handle errors
            panic!("! not followed by =, no token to generate")
        }
    }

    fn read_builtin(&mut self) -> Token {
        let start = self.position - 1;
        let mut string = String::new();

        while let Some(c) = self.peek() {
            if c.is_alphabetic() || c.is_numeric() || c == '$' || c == '_' {
                string.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Token::new((start..self.position).into(), TokenKind::Builtin(string))
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        match self.advance()? {
            '+' => Some(self.char(TokenKind::Plus)),
            '-' => Some(self.char(TokenKind::Minus)),
            '*' => Some(self.char(TokenKind::Star)),
            '/' => self.slash_or_comment(),
            ';' => Some(self.char(TokenKind::Semicolon)),
            '=' => self.equal_or_compare(),
            ',' => Some(self.char(TokenKind::Comma)),
            '{' => Some(self.char(TokenKind::OpenCurly)),
            '}' => Some(self.char(TokenKind::CloseCurly)),
            ':' => Some(self.char(TokenKind::Colon)),
            '.' => self.dot_or_dotdotdot(),
            '(' => Some(self.char(TokenKind::OpenParen)),
            ')' => Some(self.char(TokenKind::CloseParen)),
            '"' => Some(self.read_string()),
            '!' => self.read_exclamation_equal(),
            '@' => Some(self.read_builtin()),
            c if c.is_ascii_digit() => Some(self.read_number(c)),
            c if c.is_alphabetic() => Some(self.read_identifier(c)),
            c => panic!("Unknown token {c}"),
        }
    }
}
