use std::process::Command;

use codegen::Codegen;
use error::Result;
use package::Package;
use typechecker::Checker;

mod ast;
mod codegen;
mod error;
mod helpers;
mod lexer;
mod package;
mod parser;
mod pratt;
#[cfg(test)]
mod tests;
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
    let std_package = Package::from_path("std", "std".into())?;
    let std_package = std_package.parse()?;

    let main_package = Package::from_file("examples/expressions.scl".into())?;
    let main_package = main_package.parse()?;

    let mut checker = Checker::new();
    let checked_std_package = checker.add_package(&std_package, &vec![])?;
    let checked_main_package = checker.add_package(
        &main_package,
        &vec![("std".into(), checked_std_package.package_id)],
    )?;

    let all_units = checked_std_package
        .units
        .into_iter()
        .chain(checked_main_package.units)
        .collect::<Vec<_>>();

    let mut codegener = Codegen::new(all_units, checker);
    let code = codegener.generate();

    std::fs::create_dir_all("out/").unwrap();
    std::fs::write("out/a.qbe", code).unwrap();

    // Compile to asm
    let qbe_cmd = Command::new("qbe")
        .arg("-o")
        .arg("out/a.S")
        .arg("out/a.qbe")
        .output()
        .unwrap();

    if !qbe_cmd.status.success() {
        println!("qbe failed");
        println!("stderr:\n{}", String::from_utf8_lossy(&qbe_cmd.stderr));
        println!("stdout:\n{}", String::from_utf8_lossy(&qbe_cmd.stdout));

        return Ok(());
    }

    // Compile to machinecode
    let gcc_cmd = Command::new("gcc")
        .arg("-o")
        .arg("out/a.out")
        .arg("out/a.S")
        .output()
        .unwrap();

    if !gcc_cmd.status.success() {
        println!("gcc failed");
        println!("stderr:\n{}", String::from_utf8_lossy(&gcc_cmd.stderr));
        println!("stdout:\n{}", String::from_utf8_lossy(&gcc_cmd.stdout));

        return Ok(());
    }

    Ok(())
}
