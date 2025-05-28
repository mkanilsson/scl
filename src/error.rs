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
    #[error("Expected identifier before struct instantation")]
    IdentBeforeStructInstantation {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
}
