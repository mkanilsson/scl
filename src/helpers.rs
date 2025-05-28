pub fn string_join_with_or(items: &[&str]) -> String {
    match items.len() {
        0 => String::new(),
        1 => items[0].to_string(),
        2 => format!("{} or {}", items[0], items[1]),
        _ => {
            let all_except_last = items[..items.len() - 1].join(", ");
            format!("{} or {}", all_except_last, items.last().unwrap())
        }
    }
}
