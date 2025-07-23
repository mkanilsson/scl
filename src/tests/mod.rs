use std::{env, fs, path::PathBuf, process::Command};

use insta::glob;
use miette::{GraphicalReportHandler, GraphicalTheme};
use nanoid::nanoid;

use crate::{codegen::Codegen, package::Package, typechecker::Checker};

#[test]
fn runtime() {
    let std_package = Package::from_path("std", "std".into()).unwrap();
    let std_package = std_package.parse().unwrap();

    fn generate_tempfile(extension: &str) -> PathBuf {
        let name = format!("{}.{extension}", nanoid!());
        env::temp_dir().join(name)
    }

    glob!("sources/runtime/*", |file| {
        let main_package = Package::from_file(file.into()).unwrap();
        let main_package = main_package.parse().unwrap();

        let mut checker = Checker::new();
        let checked_std_package = checker.add_package(&std_package, &vec![]).unwrap();

        let checked_main_package = checker
            .add_package(&main_package, &vec![(
                "std".into(),
                checked_std_package.package_id,
            )])
            .unwrap();

        insta::assert_debug_snapshot!(checked_main_package);

        let all_units = checked_std_package
            .units
            .into_iter()
            .chain(checked_main_package.units.into_iter())
            .collect::<Vec<_>>();

        let mut codegener = Codegen::new(all_units, checker);
        let code = codegener.generate();

        insta::assert_snapshot!(code);

        let tmp = generate_tempfile("ssa");
        let asm = generate_tempfile("S");
        let executable = generate_tempfile("out");

        std::fs::write(&tmp, code).unwrap();

        let qbe_cmd = Command::new("qbe")
            .arg("-o")
            .arg(&asm)
            .arg(tmp)
            .output()
            .unwrap();

        if !qbe_cmd.status.success() {
            let stdout = String::from_utf8_lossy(&qbe_cmd.stdout);
            let stderr = String::from_utf8_lossy(&qbe_cmd.stderr);

            panic!("QBE failed:\nOut:\n{stdout}Err:\n{stderr}");
        }

        let gcc_cmd = Command::new("gcc")
            .arg("-o")
            .arg(&executable)
            .arg(&asm)
            .output()
            .unwrap();

        if !gcc_cmd.status.success() {
            let stdout = String::from_utf8_lossy(&gcc_cmd.stdout);
            let stderr = String::from_utf8_lossy(&gcc_cmd.stderr);

            panic!("gcc failed:\nOut:\n{stdout}Err:\n{stderr}");
        }

        let run_cmd = Command::new(executable).output().unwrap();

        let stdout = String::from_utf8_lossy(&run_cmd.stdout);
        let stderr = String::from_utf8_lossy(&run_cmd.stderr);

        insta::assert_snapshot!(stdout);
        insta::assert_snapshot!(stderr);
        insta::assert_snapshot!(run_cmd.status);
    });
}

#[test]
fn typecheck_errors() {
    let std_package = Package::from_path("std", "std".into()).unwrap();
    let std_package = std_package.parse().unwrap();

    glob!("sources/typecheck_error/*", |file| {
        let main_package = Package::from_file(file.into()).unwrap();
        let main_package = main_package.parse().unwrap();

        let mut checker = Checker::new();
        let checked_std_package = checker.add_package(&std_package, &vec![]).unwrap();

        let err = checker
            .add_package(&main_package, &vec![(
                "std".into(),
                checked_std_package.package_id,
            )])
            .err()
            .unwrap();

        let mut buf = String::new();
        GraphicalReportHandler::new_themed(GraphicalTheme::none())
            .render_report(&mut buf, miette::Report::new(err).as_ref())
            .unwrap();

        insta::assert_snapshot!(buf);
    });
}

#[test]
fn parse_expr() {
    glob!("sources/parse_expr/*", |file| {
        let source = fs::read_to_string(file).unwrap();
        let expr = crate::new_parser::Parser::parse_expr(&source);

        insta::assert_debug_snapshot!(expr);
    });
}

#[test]
fn parse_stmt() {
    glob!("sources/parse_stmt/*", |file| {
        let source = fs::read_to_string(file).unwrap();
        let expr = crate::new_parser::Parser::parse_stmt(&source);

        insta::assert_debug_snapshot!(expr);
    });
}
