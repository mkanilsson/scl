use lexer::Lexer;
use pratt::Pratt;

mod ast;
mod helpers;
mod lexer;
mod parser;
mod pratt;
mod token;

fn main() {
    let lexer = Lexer::new("examples/expressions.scl".into());
    let mut parser = parser::Parser::new(lexer);
    let expr = parser.parse();

    // println!("Nud Plus: {} (false)", token::TokenKind::Plus.is_nud());
    // println!(
    //     "Nud Number: {} (true)",
    //     token::TokenKind::Number(3).is_nud()
    // );
    // println!("Nud Minus: {} (false)", token::TokenKind::Minus.is_nud());
    // println!("Nud Slash: {} (false)", token::TokenKind::Slash.is_nud());
    //
    // println!("Led Plus: {} (true)", token::TokenKind::Plus.is_led());
    // println!(
    //     "Led Number: {} (false)",
    //     token::TokenKind::Number(3).is_led()
    // );
    // println!("Led Minus: {} (true)", token::TokenKind::Minus.is_led());
    // println!("Led Slash: {} (true)", token::TokenKind::Slash.is_led());
    // println!("bp Start: {:#?}", token::TokenKind::Star.binding_power());
    // println!("bp Plus: {:#?}", token::TokenKind::Plus.binding_power());
    // println!(
    //     "bp Number: {:#?}",
    //     token::TokenKind::Number(5).binding_power()
    // );

    println!("{expr:#?}");
}
