use miette::{Diagnostic, NamedSource, SourceSpan};

pub type Result<T, E = Error> = miette::Result<T, E>;

#[derive(thiserror::Error, Debug, Diagnostic)]
#[error("Scl compiler error")]
pub enum Error {
    #[error("Unexpected token")]
    UnexpectedToken {
        #[source_code]
        src: NamedSource<String>,

        #[label("expected '{expected}'")]
        span: SourceSpan,

        expected: String,
    },
    #[error("Expected '{expected}' but got '{got}'")]
    ExpectedButGot {
        #[source_code]
        src: NamedSource<String>,

        #[label("expected '{expected}'")]
        span: SourceSpan,

        expected: String,
        got: String,
    },
    #[error("Expected identifier before struct instantation")]
    IdentBeforeStructInstantation {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
    #[error("Expected proc or struct definition")]
    ExpectedProcOrStruct {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
    #[error("Unknown type '{type_name}'")]
    UnknownType {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        type_name: String,
    },

    #[error("Proc named '{name}' has already been defined")]
    ProcNameCollision {
        #[source_code]
        src: NamedSource<String>,

        #[label("original defined here")]
        original_span: SourceSpan,

        #[label("and redefined here")]
        redefined_span: SourceSpan,

        name: String,
    },
}
