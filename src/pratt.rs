pub use pratt_derive::Pratt;

use crate::{
    ast::parsed::{Expr, Stmt},
    error::Result,
    parser::{BindingPower, Parser},
};

pub trait Pratt {
    fn is_nud(&self) -> bool;
    fn nud_handler(&self) -> Option<fn(&mut Parser) -> Result<Expr>>;
    fn nud_names() -> Vec<&'static str>;

    fn is_led(&self) -> bool;
    fn led_handler(&self) -> Option<fn(&mut Parser, lhs: Expr, bp: BindingPower) -> Result<Expr>>;

    fn stmt_handler(&self) -> Option<fn(&mut Parser) -> Result<Stmt>>;

    fn binding_power(&self) -> Option<BindingPower>;
    fn name(&self) -> &'static str;
}
