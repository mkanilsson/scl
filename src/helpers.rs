use std::{collections::HashSet, hash::Hash};

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
