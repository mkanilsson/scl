use std::path::PathBuf;

use miette::{Diagnostic, NamedSource, SourceSpan};

pub type Result<T, E = Error> = miette::Result<T, E>;

#[derive(thiserror::Error, Debug, Diagnostic)]
#[error("Scl compiler error")]
pub enum Error {
    #[error("Extra token")]
    ExtraToken {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
    #[error("Invalid token")]
    InvalidToken {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
    #[error("Unexpected token")]
    UnexpectedToken {
        #[source_code]
        src: NamedSource<String>,

        #[label("expected '{expected}'")]
        span: SourceSpan,

        expected: String,
    },
    #[error("Expected one of {expected}")]
    ExpectedOneOf {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
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
    #[error("Unknown type '{type_name}'")]
    UnknownType {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        type_name: String,
    },
    #[error("Unknown proc '{proc_name}'")]
    UnknownProc {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        proc_name: String,
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
    #[error("Proc param expected to be '{expected}' but got '{got}'")]
    ProcCallParamTypeMismatch {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        expected: String,
        got: String,
    },
    #[error("Unknown builtin '{name}'")]
    UnknownBuiltin {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        name: String,
    },
    #[error("Builtin '{name}' expects '{arg_type}' as argument {arg_index}")]
    BuiltinExpectsArgAtToBe {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        name: &'static str,
        arg_type: &'static str,
        arg_index: u32,
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
    #[error("'{got}' is not a struct")]
    MemberAccessNotAStruct {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        got: String,
    },
    #[error("'{struct_name}' has no field '{field_name}'")]
    MemberAccessUnknownField {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        struct_name: String,
        field_name: String,
    },
    #[error("Expected a '{root_file_name}.scl' in '{path}'")]
    ExpectedRootFile {
        root_file_name: &'static str,
        path: PathBuf,
    },
    #[error(
        "Module defined twice, both '{module_name}.scl' and '{module_name}/mod.scl' exists in '{path}'"
    )]
    ModuleDefinedTwice { module_name: String, path: PathBuf },
    #[error("Can't find module '{module_name}' in '{base_name}'")]
    ModuleNotFound {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        module_name: String,
        base_name: String,
    },
    #[error("Can't find proc or struct '{wanted_name}' in '{module_name}'")]
    ProcOrStructNotFound {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        wanted_name: String,
        module_name: String,
    },
    #[error("Block expects to return a value but nothing is yielded")]
    BlockRequiresValue {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },

    #[error("If blocks type mismatch")]
    IfTypeMismatch {
        #[source_code]
        src: NamedSource<String>,

        #[label("this evaluates to '{true_block_type}'")]
        true_block_span: SourceSpan,
        true_block_type: String,

        #[label("but this evaluates to '{false_block_type}'")]
        false_block_span: SourceSpan,
        false_block_type: String,
    },

    #[error("Can't cast from '{got}' to '{wanted}'")]
    InvalidCast {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,

        got: String,
        wanted: String,
    },

    #[error("Can't assign to an rvalue")]
    CantAssignToRValue {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
    #[error("Can't dereference a non-ptr")]
    DerefNonPtr {
        #[source_code]
        src: NamedSource<String>,

        #[label("this evaluates to '{type_name}'")]
        span: SourceSpan,
        type_name: String,
    },
    #[error("Generic already defined with another type")]
    GenericAlreadyDefinedWithAnotherType {
        #[source_code]
        src: NamedSource<String>,

        #[label("defined here as '{defined_name}'")]
        defined_span: SourceSpan,
        defined_name: String,

        #[label("infered as '{infered_name}' here")]
        infered_span: SourceSpan,
        infered_name: String,
    },
    #[error("Array instantiation has different types")]
    ArrayInstantiationDifferentTypes {
        #[source_code]
        src: NamedSource<String>,

        #[label("this evaluates to '{first_type_name}'")]
        first_span: SourceSpan,
        first_type_name: String,

        #[label("this evaluates to '{second_type_name}'")]
        second_span: SourceSpan,
        second_type_name: String,
    },
    #[error("Array access on non-array")]
    ArrayAccessOnNonArray {
        #[source_code]
        src: NamedSource<String>,

        #[label("this evaluates to '{type_name}'")]
        span: SourceSpan,
        type_name: String,
    },
    #[error("Array access with non-integer index")]
    ArrayAccessWithNonIntegerIndex {
        #[source_code]
        src: NamedSource<String>,

        #[label("this evaluates to '{type_name}'")]
        span: SourceSpan,
        type_name: String,
    },
    #[error("'this' is only allowed in impl procs")]
    ThisOnImplBlock {
        #[source_code]
        src: NamedSource<String>,

        #[label("here")]
        span: SourceSpan,
    },
}
