use std::{path::PathBuf, process::Command};

use crate::{
    codegen::Codegen,
    env::Env,
    error::{Error, Result},
    generics::Generic,
    package::{CheckedPackage, Package},
    typechecker::{Checker, ast::CheckedProc},
};

pub struct Compiler {
    packages: Vec<CheckedPackage>,
    checker: Checker,
    stdlib: CheckedPackage,
    generic_procs: Vec<CheckedProc>,
}

impl Compiler {
    pub fn build_from_file(file: PathBuf) -> Result<String> {
        let mut compiler = Self::new()?;
        compiler.add_package_from_file(file)?;
        compiler.generate_generic_procs();

        println!("Typechecker success!");
        Self::write_to_disk(compiler.to_qbe());
        Self::compile();
        Ok("out/a.out".to_string())
    }

    fn new() -> Result<Self> {
        let path = match Env::stdlib_path() {
            Some(path) => path,
            None => return Err(Error::NoStdLibPath),
        };

        // let std_package = match Package::from_path(path.into()) {
        //     Ok(std_package) => std_package,
        //     Err(_) => return Err(Error::CantCompileStdLib),
        // };
        //
        // let std_package = match std_package.parse() {
        //     Ok(std_package) => std_package,
        //     Err(_) => return Err(Error::CantCompileStdLib),
        // };

        let std_package = Package::from_path(path.into())?;
        let std_package = std_package.parse()?;

        let mut checker = Checker::new();

        // let std_package = match checker.add_package(std_package, &[]) {
        //     Ok(checked_package) => checked_package,
        //     Err(_) => return Err(Error::CantCompileStdLib),
        // };

        let std_package = checker.add_package(std_package, &[])?;

        Ok(Self {
            checker,
            stdlib: std_package,
            packages: vec![],
            generic_procs: vec![],
        })
    }

    fn add_package_from_file(&mut self, path: PathBuf) -> Result<()> {
        let main_package = Package::from_file(path)?;
        let main_package = main_package.parse()?;

        let main_package = self
            .checker
            .add_package(main_package, &[("std".into(), self.stdlib.package_id)])?;

        self.packages.push(main_package);

        Ok(())
    }

    fn generate_generic_procs(&mut self) {
        self.generic_procs.extend(Generic::transform(&self.checker));
    }

    fn to_qbe(self) -> String {
        let mut all_units = self.stdlib.units;
        for package in self.packages {
            all_units.extend(package.units);
        }

        let mut codegener = Codegen::new(all_units, self.generic_procs, self.checker);
        codegener.generate()
    }

    fn write_to_disk(code: String) {
        std::fs::create_dir_all("out/").unwrap();
        std::fs::write("out/a.qbe", code).unwrap();
    }

    fn compile() {
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

            panic!("qbe failed");
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

            panic!("gcc failed");
        }
    }
}
