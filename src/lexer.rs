use logos::{Logos, SpannedIter};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"//.*\n")]
pub enum Token<'source> {
    #[regex("-*[0-9]+")]
    Number(&'source str),
    #[regex("[a-zA-Z]+[a-zA-Z0-9_]*")]
    Identifier(&'source str),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| { let slice = lex.slice(); &slice[1..slice.len() - 1] })]
    String(&'source str),
    #[regex("@[a-zA-Z]+[a-zA-Z0-9_]*", |lex| &lex.slice()[1..])]
    Builtin(&'source str),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token(")")]
    OpenParen,
    #[token("(")]
    CloseParen,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("let")]
    Let,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("&")]
    Ampersand,
    #[token("=")]
    Equal,
    #[token(";")]
    Semicolon,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    ExclamationEqual,
    #[token("!")]
    Exclamation,
    #[token("{")]
    OpenCurly,
    #[token("}")]
    CloseCurly,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("...")]
    DotDotDot,
    #[token("ret")]
    Ret,
    #[token("proc")]
    Proc,
    #[token("extern")]
    Extern,
    #[token("struct")]
    Struct,
    #[token("use")]
    Use,
    #[token("as")]
    As,
    #[token("while")]
    While,
    #[token("defer")]
    Defer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidToken(usize, usize),
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| {
            let Ok(token) = token else {
                return Err(LexicalError::InvalidToken(span.start, span.end));
            };

            Ok((span.start, token, span.end))
        })
    }
}
