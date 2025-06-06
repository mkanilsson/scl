use codegen::Codegen;
use error::Result;
use lexer::Lexer;
use typechecker::Checker;

mod ast;
mod codegen;
mod error;
mod helpers;
mod lexer;
mod parser;
mod pratt;
mod token;
mod typechecker;

fn main() -> miette::Result<()> {
    let something_or_error = run();
    if let Err(err) = something_or_error {
        let me: miette::Error = err.into();
        return Err(me);
    }

    Ok(())
}

fn run() -> Result<()> {
    let lexer = Lexer::new("examples/expressions.scl".into());
    let parser = parser::Parser::new(lexer);
    let unit = parser.parse()?;
    let mut checker = Checker::new(unit);
    let checked_unit = checker.check()?;
    println!("{checked_unit:#?}");

    let mut codegener = Codegen::new(checked_unit, checker.types);
    println!("{}", codegener.generate());

    Ok(())
}
