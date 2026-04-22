use super::Grammar;

pub fn extend_input(grammar: &mut Grammar, input: &str, token: &str) -> String {
    if input.ends_with(char::is_whitespace) || token.starts_with(char::is_whitespace) {
        return format!("{}{}", input, token);
    }

    if input.is_empty() {
        return token.to_string();
    }

    let no_space = format!("{}{}", input, token);
    let with_space = format!("{} {}", input, token);
    if should_preserve_separator(input, token) {
        return with_space;
    }

    if concatenation_preserves_tokens(grammar, input, token, &no_space) {
        return no_space;
    }

    if concatenation_preserves_tokens(grammar, input, token, &with_space) {
        return with_space;
    }

    let last = input.chars().next_back();
    let first = token.chars().next();
    let needs_space = matches!((last, first), (Some(l), Some(r)) if needs_separator(grammar, l, r));
    if needs_space {
        format!("{} {}", input, token)
    } else {
        no_space
    }
}

fn should_preserve_separator(input: &str, token: &str) -> bool {
    let Some(left) = input.chars().next_back() else {
        return false;
    };
    let Some(right) = token.chars().next() else {
        return false;
    };

    if !right.is_ascii_alphanumeric() {
        return false;
    }

    if left.is_ascii_alphanumeric() {
        return true;
    }

    !matches!(left, ':' | '=')
}

fn concatenation_preserves_tokens(
    grammar: &mut Grammar,
    input: &str,
    token: &str,
    combined: &str,
) -> bool {
    let Ok(left) = grammar.tokenize(input) else {
        return false;
    };
    let Ok(right) = grammar.tokenize(token) else {
        return false;
    };
    let Ok(joined) = grammar.tokenize(combined) else {
        return false;
    };

    let expected: Vec<_> = left
        .iter()
        .chain(right.iter())
        .map(|segment| (segment.as_str().to_string(), segment.is_partial_special))
        .collect();
    let actual: Vec<_> = joined
        .iter()
        .map(|segment| (segment.as_str().to_string(), segment.is_partial_special))
        .collect();
    expected == actual
}

fn needs_separator(grammar: &mut Grammar, left: char, right: char) -> bool {
    let delim = |ch: char| ch.is_whitespace() || grammar.delimiters.contains(&ch);
    !delim(left) && !delim(right) && left.is_ascii_alphanumeric() && right.is_ascii_alphanumeric()
}
