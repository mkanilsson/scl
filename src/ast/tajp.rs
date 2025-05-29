use crate::ast::parsed::Ident;

#[derive(Debug, Clone)]
pub enum Type {
    Named(Ident),
}
