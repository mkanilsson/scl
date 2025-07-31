use miette::NamedSource;

use crate::{
    ast::parsed::TranslationUnit,
    error::{Error, Result},
    helpers::{self, find_duplicate},
    parser::Parser,
    typechecker::{ast::CheckedTranslationUnit, module::ModuleId},
};
use std::{fs, path::PathBuf};

#[derive(Debug)]
pub struct Module {
    name: String,
    path: PathBuf,
    children: Vec<Module>,
    source: NamedSource<String>,
}

#[derive(Debug)]
pub struct Package {
    name: String,
    path: PathBuf,
    modules: Vec<Module>,
    source: NamedSource<String>,
}

// TODO: Handle errors
impl Package {
    pub fn from_path(name: impl Into<String>, path: PathBuf) -> Result<Self> {
        let path = Self::to_absolute_path(path.join("src"));
        let modules = Self::find_modules(path.clone())?;

        let path = path.join("lib.scl");
        let source = fs::read_to_string(&path).unwrap();

        Ok(Self {
            name: name.into(),
            modules,
            source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
            path,
        })
    }

    pub fn from_file(path: PathBuf) -> Result<Self> {
        let path = Self::to_absolute_path(path);

        let source = fs::read_to_string(&path).unwrap();

        Ok(Self {
            name: "main".to_string(),
            modules: vec![],
            source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
            path,
        })
    }

    fn find_modules(path: PathBuf) -> Result<Vec<Module>> {
        Ok(Self::build_module_from_directory("", path, true)?.children)
    }

    fn build_module_from_directory(name: &str, path: PathBuf, first: bool) -> Result<Module> {
        let root_file_name = if first { "lib" } else { "mod" };

        let mut root_file_found = false;

        let mut children = vec![];
        for entry in std::fs::read_dir(&path).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();

            let file_name = entry.file_name().into_string().unwrap();

            if path.is_dir() {
                children.push(Self::build_module_from_directory(&file_name, path, false)?);
            } else if let Some(file_name) = file_name.strip_suffix(".scl") {
                if file_name == root_file_name {
                    root_file_found = true;
                    continue;
                }

                let source = fs::read_to_string(&path).unwrap();

                children.push(Module {
                    children: vec![],
                    name: file_name.to_string(),
                    source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
                    path,
                });
            }
        }

        if !root_file_found {
            return Err(Error::ExpectedRootFile {
                path,
                root_file_name,
            });
        }

        if let Some(module) = find_duplicate(
            children
                .iter()
                .map(|c| &c.name)
                .collect::<Vec<_>>()
                .as_slice(),
        ) {
            return Err(Error::ModuleDefinedTwice {
                module_name: (**module).clone(),
                path,
            });
        }

        let path = path.join(format!("{}.scl", root_file_name));
        let source = fs::read_to_string(&path).unwrap();

        Ok(Module {
            name: name.into(),
            children,
            source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
            path,
        })
    }

    fn to_absolute_path(path: PathBuf) -> PathBuf {
        if path.is_absolute() {
            return path.to_path_buf();
        }

        let cwd = std::env::current_dir().unwrap();
        cwd.join(path)
    }

    pub fn parse(self) -> Result<ParsedPackage> {
        let module = Self::parse_module(Module {
            children: self.modules,
            path: self.path,
            name: self.name,
            source: self.source,
        })?;

        Ok(ParsedPackage {
            name: module.name,
            path: module.path,
            modules: module.children,
            unit: module.unit,
            source: module.source,
        })
    }

    fn parse_module(module: Module) -> Result<ParsedModule> {
        let unit = Parser::parse_translation_unit(&module.source)?;

        let mut children = vec![];

        for child in module.children {
            children.push(Self::parse_module(child)?);
        }

        Ok(ParsedModule {
            name: module.name,
            path: module.path,
            unit,
            children,
            source: module.source,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedPackage {
    pub name: String,
    pub unit: TranslationUnit,
    pub path: PathBuf,
    pub modules: Vec<ParsedModule>,
    pub source: NamedSource<String>,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub name: String,
    pub unit: TranslationUnit,
    pub path: PathBuf,
    pub children: Vec<ParsedModule>,
    pub source: NamedSource<String>,
}

#[derive(Debug)]
pub struct CheckedPackage {
    pub package_id: ModuleId,
    pub units: Vec<CheckedTranslationUnit>,
}

impl CheckedPackage {
    pub fn new(package_id: ModuleId, units: Vec<CheckedTranslationUnit>) -> Self {
        Self { package_id, units }
    }
}
