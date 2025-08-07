use serde::Deserialize;

#[derive(Deserialize)]
pub struct PackageFile {
    pub package: Package,
}

#[derive(Deserialize)]
pub struct Package {
    pub name: String,

    #[serde(rename = "type")]
    pub tajp: PackageType,
}

#[derive(Deserialize, Default)]
pub enum PackageType {
    #[default]
    #[serde(rename = "exec")]
    Exec,

    #[serde(rename = "lib")]
    Lib,
}

impl PackageType {
    pub fn to_path(&self) -> &'static str {
        match self {
            PackageType::Exec => "main.scl",
            PackageType::Lib => "lib.scl",
        }
    }
}
