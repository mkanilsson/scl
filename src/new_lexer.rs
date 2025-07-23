use logos::{Logos, SpannedIter};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex("-*[0-9]+", |lex| lex.slice().to_string())]
    Number(String),
    #[regex("[a-zA-Z]+[a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| { let slice = lex.slice(); slice[1..slice.len() - 1].to_string() })]
    String(String),
    #[regex("@[a-zA-Z]+[a-zA-Z0-9_]*", |lex| lex.slice()[1..].to_string())]
    Builtin(String),
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
    #[token(">")]
    GreaterThan,
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
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    #[default]
    InvalidToken,
}

impl From<()> for LexicalError {
    fn from(_: ()) -> Self {
        Self::InvalidToken
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}
