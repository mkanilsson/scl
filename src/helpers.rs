use std::{
    collections::HashSet,
    env,
    fmt::Display,
    fs::{self, ReadDir},
    hash::Hash,
    path::{Path, PathBuf},
};

use crate::error::{Error, Result};

pub fn string_join_with_or<T: AsRef<str> + ToString + Display>(items: &[T]) -> String {
    string_join_with(items, "or")
}

pub fn string_join_with_and<T: AsRef<str> + ToString + Display>(items: &[T]) -> String {
    string_join_with(items, "and")
}

pub fn string_join_with<T: AsRef<str> + ToString + Display>(items: &[T], last: &str) -> String {
    match items.len() {
        0 => String::new(),
        1 => items[0].to_string(),
        2 => format!("{} {last} {}", items[0], items[1]),
        _ => {
            let all_except_last = items[..items.len() - 1]
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} {last} {}", all_except_last, items.last().unwrap())
        }
    }
}

pub fn find_duplicate<T>(items: &[T]) -> Option<&T>
where
    T: Hash + Eq,
{
    let mut existing = HashSet::new();

    for item in items {
        if existing.contains(&item) {
            return Some(item);
        }

        existing.insert(item);
    }

    None
}

pub fn relative_path(path: &Path) -> String {
    let current_dir = env::current_dir().unwrap();
    if path.starts_with(&current_dir) {
        path.strip_prefix(current_dir)
            .unwrap_or(path)
            .to_string_lossy()
            .to_string()
    } else {
        path.to_string_lossy().to_string()
    }
}

pub fn expand(path: String) -> Option<String> {
    let home = env::var("HOME").ok()?;

    if path.starts_with("~/") || path.starts_with("$HOME/") {
        Some(path.replacen("~", &home, 1).replacen("$HOME", &home, 1))
    } else {
        Some(path)
    }
}

pub fn safe_read<P: AsRef<Path> + Into<PathBuf>>(path: P) -> Result<String> {
    match fs::read_to_string(&path) {
        Ok(content) => Ok(content),
        Err(inner) => Err(Error::IoErrorWithPath {
            path: path.into(),
            error: inner,
        }),
    }
}

pub fn safe_read_dir<P: AsRef<Path> + Into<PathBuf>>(path: P) -> Result<ReadDir> {
    match fs::read_dir(&path) {
        Ok(content) => Ok(content),
        Err(inner) => Err(Error::IoErrorWithPath {
            path: path.into(),
            error: inner,
        }),
    }
}
