// This should be removed in the future when the errors are accumulated into
// a Vec of errors, making the error variant a lot smaller
#![allow(clippy::result_large_err)]

use std::process::{Command, Stdio};

use clap::Parser;
use colored::Colorize;
use error::Result;

use crate::env::Env;

mod ast;
mod cli;
mod codegen;
mod compiler;
mod env;
mod error;
mod helpers;
mod lexer;
mod package;
mod parser;
#[cfg(test)]
mod tests;
mod typechecker;

fn main() -> miette::Result<()> {
    let cmd = cli::Cli::parse();

    match cmd {
        cli::Cli::Build { file } => {
            compiler::Compiler::build_from_file(file)?;
        }
        cli::Cli::Run { file } => {
            let executable = compiler::Compiler::build_from_file(file)?;
            Command::new(executable)
                .stdin(Stdio::inherit())
                .stdout(Stdio::inherit())
                .output()
                .unwrap();
        }
        cli::Cli::Env => {
            run_env_cmd()?;
        }
    }

    Ok(())
}

fn run_env_cmd() -> Result<()> {
    println!("{}:", "Env".magenta());
    println!(
        "    {}: {}",
        "SCL_STDLIB_PATH".blue().bold(),
        match Env::stdlib_path() {
            Some(path) => path.green(),
            None => "<MISSING>".red().italic(),
        }
    );

    println!(
        "    {}: {}",
        "QBE".blue().bold(),
        if Env::has_qbe() {
            "Found".green()
        } else {
            "<MISSING>".red().italic()
        }
    );

    println!(
        "    {}: {}",
        "GCC".blue().bold(),
        if Env::has_gcc() {
            "Found".green()
        } else {
            "<MISSING>".red().italic()
        }
    );

    Ok(())
}
