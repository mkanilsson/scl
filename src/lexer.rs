use std::{path::PathBuf, process::id};

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
        self.content[self.position..].chars().next()
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.peek();
        if let Some(_) = next {
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

        Token::new(
            (start..self.position).into(),
            TokenKind::Number(string.parse().unwrap()),
        )
    }

    fn read_identifier(&mut self, current: char) -> Token {
        let start = self.position;
        let mut string = String::from(current);

        while let Some(c) = self.peek() {
            if c.is_alphabetic() || c == '$' {
                string.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Keywords
        let kind = match string.as_str() {
            "let" => TokenKind::Let,
            _ => TokenKind::Identifier(string),
        };

        Token::new((start..self.position).into(), kind)
    }

    fn char(&self, kind: TokenKind) -> Token {
        Token::new((self.position - 1..self.position).into(), kind)
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
            '/' => Some(self.char(TokenKind::Slash)),
            ';' => Some(self.char(TokenKind::Semicolon)),
            '=' => Some(self.char(TokenKind::Equal)),
            ',' => Some(self.char(TokenKind::Comma)),
            '{' => Some(self.char(TokenKind::OpenCurly)),
            '}' => Some(self.char(TokenKind::CloseCurly)),
            ':' => Some(self.char(TokenKind::Colon)),
            '.' => Some(self.char(TokenKind::Dot)),
            c if c.is_ascii_digit() => Some(self.read_number(c)),
            c if c.is_alphabetic() => Some(self.read_identifier(c)),
            c => panic!("Unknown token {c}"),
        }
    }
}
