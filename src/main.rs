use lexer::Lexer;

mod ast;
mod error;
mod helpers;
mod lexer;
mod parser;
mod pratt;
mod token;

fn main() -> miette::Result<()> {
    let lexer = Lexer::new("examples/expressions.scl".into());
    let parser = parser::Parser::new(lexer);
    let expr_or_error = parser.parse();

    match expr_or_error {
        Ok(expr) => println!("{expr:#?}"),
        Err(err) => {
            let me: miette::Error = err.into();
            return Err(me);
        }
    }

    Ok(())
}
