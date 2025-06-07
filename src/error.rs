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
    #[error("Expected proc or struct definition or extern proc")]
    ExpectedProcStructExtern {
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
    #[error("Proc param named '{name}' has already been defined")]
    ProcParmNameCollision {
        #[source_code]
        src: NamedSource<String>,

        #[label("original defined here")]
        original_span: SourceSpan,

        #[label("and redefined here")]
        redefined_span: SourceSpan,

        name: String,
    },
    #[error("Proc expects void but ret has value")]
    ReturnShouldntHaveValue {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
    #[error("Proc expects '{name}' but ret has no value")]
    ReturnShouldHaveValue {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        name: String,
    },
    #[error("Proc returns '{return_type}' but ret expr is '{actual_type}'")]
    ReturnValueDoesntMatch {
        #[source_code]
        src: NamedSource<String>,

        #[label("return type defined here")]
        return_type_span: SourceSpan,

        #[label("evaluates to '{actual_type}'")]
        expr_span: SourceSpan,

        return_type: String,
        actual_type: String,
    },
    #[error("No proc or variable named '{ident}' found in scope")]
    UnknownIdent {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        ident: String,
    },
    #[error("'{value}' is not a valid '{type_name}'")]
    InvalidNumber {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        value: String,
        type_name: String,
    },
    #[error("BinOp sides don't match")]
    BinOpSidesMismatch {
        #[source_code]
        src: NamedSource<String>,

        #[label("this evaluates to '{lhs_type_name}'")]
        lhs_span: SourceSpan,

        #[label("and this evaluates to '{rhs_type_name}'")]
        rhs_span: SourceSpan,

        lhs_type_name: String,
        rhs_type_name: String,
    },
}
