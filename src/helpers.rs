use std::{collections::HashSet, env, hash::Hash, path::Path};

pub fn string_join_with_or(items: &[&str]) -> String {
    string_join_with(items, "or")
}

pub fn string_join_with_and(items: &[&str]) -> String {
    string_join_with(items, "and")
}

pub fn string_join_with(items: &[&str], last: &str) -> String {
    match items.len() {
        0 => String::new(),
        1 => items[0].to_string(),
        2 => format!("{} {last} {}", items[0], items[1]),
        _ => {
            let all_except_last = items[..items.len() - 1].join(", ");
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
