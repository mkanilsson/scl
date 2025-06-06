use error::Result;
use lexer::Lexer;
use typechecker::Checker;

mod ast;
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
    println!("{checker:#?}");
    Ok(())
}
