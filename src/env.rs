use std::process::Command;

use crate::helpers;

pub struct Env;

impl Env {
    pub fn stdlib_path() -> Option<String> {
        helpers::expand(std::env::var("SCL_STDLIB_PATH").ok()?)
    }

    pub fn has_gcc() -> bool {
        Command::new("gcc").arg("--version").output().is_ok()
    }

    pub fn has_qbe() -> bool {
        Command::new("qbe").arg("-h").output().is_ok()
    }
}
