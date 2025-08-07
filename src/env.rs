use crate::helpers;

pub struct Env;

impl Env {
    pub fn stdlib_path() -> Option<String> {
        helpers::expand(std::env::var("SCL_STDLIB_PATH").ok()?)
    }
}
