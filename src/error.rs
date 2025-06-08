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
    #[error(
        "Proc expects {}{expected} argument{} but got {got}",
        if *variadic { "at least " } else { "" },
        if *expected != 1 { "s" } else { "" }
    )]
    ProcCallParamCountMismatch {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        expected: usize,
        got: usize,

        variadic: bool,
    },
    #[error("Unknown builtin '{name}'")]
    UnknownBuiltin {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        name: String,
    },
    #[error(
        "Builtin '{name}' expects {}{expected} argument{} but got {got}",
        if *variadic { "at least " } else { "" },
        if *expected != 1 { "s" } else { "" }
    )]
    BuiltinParamCountMismatch {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
        name: String,

        expected: usize,
        got: usize,

        variadic: bool,
    },
    #[error("Struct field named '{name}' has already been defined")]
    StructFieldNameCollision {
        #[source_code]
        src: NamedSource<String>,

        #[label("original defined here")]
        original_span: SourceSpan,

        #[label("and redefined here")]
        redefined_span: SourceSpan,

        name: String,
    },
    #[error("Trying to instantiate a non struct type")]
    StructInstantiationOnNonStruct {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
    #[error("Struct '{struct_name}' has no field '{field_name}'")]
    StructInstantiationFieldDoesntExist {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        struct_name: String,
        field_name: String,
    },
    #[error("Field '{field_name}' has already been declared")]
    StructInstantiationFieldAlreadyDeclared {
        #[source_code]
        src: NamedSource<String>,

        #[label("original declaration here")]
        original_span: SourceSpan,

        #[label("and redeclared here")]
        redefined_span: SourceSpan,

        field_name: String,
    },
    #[error("Struct '{struct_name}' require field{} {fields}", if *multiple { "s" } else { "" })]
    StructInstantiationMissingFields {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        struct_name: String,
        fields: String,
        multiple: bool,
    },
    #[error("'{struct_name}.{field_name}' expects '{expected}' but got '{got}'")]
    StructInstantiationFieldTypeMismatch {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        struct_name: String,
        field_name: String,
        expected: String,
        got: String,
    },
}
