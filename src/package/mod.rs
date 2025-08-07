mod file;

use miette::NamedSource;

use crate::{
    ast::parsed::TranslationUnit,
    error::{Error, Result},
    helpers::{self, find_duplicate},
    parser::Parser,
    typechecker::{ast::CheckedTranslationUnit, module::ModuleId},
};
use std::path::PathBuf;

#[derive(Debug)]
pub struct Module {
    name: String,
    children: Vec<Module>,
    source: NamedSource<String>,
}

#[derive(Debug)]
pub struct Package {
    name: String,
    modules: Vec<Module>,
    source: NamedSource<String>,
}

// TODO: Handle errors
impl Package {
    pub fn from_path(path: PathBuf) -> Result<Self> {
        let path = Self::to_absolute_path(path);
        let package_file = Self::parse_scl_toml_file(path.clone())?;

        let src_path = path.join("src");
        let modules = Self::find_modules(&src_path)?;

        let path = src_path.join(package_file.package.tajp.to_path());
        let source = helpers::safe_read(&path)?;

        Ok(Self {
            name: package_file.package.name,
            modules,
            source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
        })
    }

    pub fn from_file(path: PathBuf) -> Result<Self> {
        let path = Self::to_absolute_path(path);

        let source = helpers::safe_read(&path)?;

        Ok(Self {
            name: "main".to_string(),
            modules: vec![],
            source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
        })
    }

    fn parse_scl_toml_file(path: PathBuf) -> Result<file::PackageFile> {
        let path = path.join("scl.toml");
        let content = helpers::safe_read(&path)?;
        let package_file: file::PackageFile = toml::from_str(&content).unwrap();

        Ok(package_file)
    }

    fn find_modules(path: &PathBuf) -> Result<Vec<Module>> {
        Ok(Self::build_module_from_directory("", path, true)?.children)
    }

    fn build_module_from_directory(name: &str, path: &PathBuf, first: bool) -> Result<Module> {
        let root_file_name = if first { "lib" } else { "mod" };

        let mut root_file_found = false;

        let mut children = vec![];
        for entry in helpers::safe_read_dir(&path)? {
            let entry = entry?;
            let path = entry.path();

            let file_name = entry.file_name().into_string().unwrap();

            if path.is_dir() {
                children.push(Self::build_module_from_directory(&file_name, &path, false)?);
            } else if let Some(file_name) = file_name.strip_suffix(".scl") {
                if file_name == root_file_name {
                    root_file_found = true;
                    continue;
                }

                let source = helpers::safe_read(&path)?;

                children.push(Module {
                    children: vec![],
                    name: file_name.to_string(),
                    source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
                });
            }
        }

        if !root_file_found {
            return Err(Error::ExpectedRootFile {
                path: path.clone(),
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
                path: path.clone(),
            });
        }

        let path = path.join(format!("{}.scl", root_file_name));
        let source = helpers::safe_read(&path)?;

        Ok(Module {
            name: name.into(),
            children,
            source: NamedSource::new(helpers::relative_path(&path), source.to_string()),
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
            name: self.name,
            source: self.source,
        })?;

        Ok(ParsedPackage {
            base_module: module,
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
            unit,
            children,
            source: module.source,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedPackage {
    pub base_module: ParsedModule,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub name: String,
    pub unit: TranslationUnit,
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
