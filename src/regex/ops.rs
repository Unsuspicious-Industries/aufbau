//! Operations on regular expressions.

use crate::debug_trace;

use super::{PrefixStatus, Regex, nfa};

// ===== Predicates =====
pub fn is_empty(r: &Regex) -> bool {
    match r {
        Regex::Empty => true,
        Regex::Concat(a, b) => is_empty(a) || is_empty(b),
        Regex::Union(a, b) => is_empty(a) && is_empty(b),
        _ => false,
    }
}

pub fn is_nullable(r: &Regex) -> bool {
    match r {
        Regex::Epsilon | Regex::Star(_) => true,
        Regex::Concat(a, b) => is_nullable(a) && is_nullable(b),
        Regex::Union(a, b) => is_nullable(a) || is_nullable(b),
        _ => false,
    }
}

// ===== Simplification =====
pub fn simplify(r: &Regex) -> Regex {
    match r {
        Regex::Concat(a, b) => match (simplify(a), simplify(b)) {
            (Regex::Empty, _) | (_, Regex::Empty) => Regex::Empty,
            (Regex::Epsilon, s) | (s, Regex::Epsilon) => s,
            (a, b) => Regex::cat(a, b),
        },
        Regex::Union(a, b) => match (simplify(a), simplify(b)) {
            (Regex::Empty, s) | (s, Regex::Empty) => s,
            (a, b) if a == b => a,
            (a, b) => Regex::or(a, b),
        },
        Regex::Star(r) => match simplify(r) {
            Regex::Empty | Regex::Epsilon => Regex::Epsilon,
            s => Regex::star(s),
        },
        r => r.clone(),
    }
}

// ===== Derivatives =====
pub fn deriv(r: &Regex, c: char) -> Regex {
    let d = match r {
        Regex::Empty | Regex::Epsilon => Regex::Empty,
        Regex::Char(ch) => {
            if *ch == c {
                Regex::Epsilon
            } else {
                Regex::Empty
            }
        }
        Regex::Range(lo, hi) => {
            if c >= *lo && c <= *hi {
                Regex::Epsilon
            } else {
                Regex::Empty
            }
        }
        Regex::Union(a, b) => Regex::or(deriv(a, c), deriv(b, c)),
        Regex::Concat(a, b) if is_nullable(a) => {
            Regex::or(Regex::cat(deriv(a, c), (**b).clone()), deriv(b, c))
        }
        Regex::Concat(a, b) => Regex::cat(deriv(a, c), (**b).clone()),
        Regex::Star(r) => Regex::cat(deriv(r, c), Regex::Star(r.clone())),
    };
    debug_trace!("deriv", "∂{}/∂{} = {}", r, c, d);
    d
}

pub fn derivative(r: &Regex, s: &str) -> Regex {
    s.chars().fold(r.clone(), |r, c| deriv(&r, c))
}

pub fn matches(r: &Regex, s: &str) -> bool {
    let nfa = nfa::NFA::from(r.clone());
    let mut current_states = nfa.epsilon_closure(vec![nfa.start]);

    for c in s.chars() {
        let mut next_states = Vec::new();
        for &state in &current_states {
            for (symbol, to) in &nfa.states[state].transitions {
                if *symbol == c {
                    next_states.push(*to);
                }
            }
        }
        if next_states.is_empty() {
            return false;
        }
        current_states = nfa.epsilon_closure(next_states);
    }

    current_states.contains(&nfa.accept)
}

pub fn match_len(r: &Regex, s: &str) -> Option<usize> {
    let nfa = nfa::NFA::from(r.clone());

    let mut current_states = nfa.epsilon_closure(vec![nfa.start]);
    let mut max_len: Option<usize> = None;

    if current_states.contains(&nfa.accept) {
        max_len = Some(0);
    }

    for (i, c) in s.chars().enumerate() {
        let mut next_states = Vec::new();
        for &state in &current_states {
            for (symbol, to) in &nfa.states[state].transitions {
                if *symbol == c {
                    next_states.push(*to);
                }
            }
        }
        if next_states.is_empty() {
            break;
        }
        current_states = nfa.epsilon_closure(next_states);
        if current_states.contains(&nfa.accept) {
            max_len = Some(i + 1);
        }
    }

    max_len
}

pub fn prefix_match(r: &Regex, s: &str) -> PrefixStatus {
    let d = derivative(r, s).simplify();
    if is_empty(&d) {
        PrefixStatus::NoMatch
    } else if matches!(&d, Regex::Empty) {
        PrefixStatus::Complete
    } else if is_nullable(&d) {
        PrefixStatus::Extensible(d)
    } else {
        PrefixStatus::Prefix(d)
    }
}

// ===== Cartesian Product =====
/// Compute cartesian product: [[a,b], [x,y]] -> [ax, ay, bx, by]
pub fn product(choices: Vec<Vec<Regex>>) -> Vec<Regex> {
    choices.into_iter().fold(vec![Regex::Epsilon], |acc, alts| {
        acc.into_iter()
            .flat_map(|p| alts.iter().map(move |r| Regex::cat(p.clone(), r.clone())))
            .collect()
    })
}

/// Product with index tracking: returns (alt_indices, regex)
pub fn product_traced(choices: Vec<Vec<Regex>>) -> Vec<(Vec<usize>, Regex)> {
    choices
        .into_iter()
        .fold(vec![(vec![], Regex::Epsilon)], |acc, alts| {
            acc.into_iter()
                .flat_map(|(idx, p)| {
                    alts.iter().enumerate().map(move |(i, r)| {
                        let mut v = idx.clone();
                        v.push(i);
                        (v, Regex::cat(p.clone(), r.clone()))
                    })
                })
                .collect()
        })
}

// ===== Utilities =====
pub fn unwrap_star(r: &Regex) -> Regex {
    if let Regex::Star(r) = r {
        (**r).clone()
    } else {
        r.clone()
    }
}

pub fn to_pattern(r: &Regex) -> String {
    match r {
        Regex::Empty => "(?!)".into(),
        Regex::Epsilon => String::new(),
        Regex::Char(c) => {
            if ".*+?|()[]{}\\^$".contains(*c) {
                format!("\\{c}")
            } else {
                c.to_string()
            }
        }
        Regex::Range(a, b) if a == b => Regex::Char(*a).to_pattern(),
        Regex::Range(a, b) => format!("[{a}-{b}]"),
        Regex::Concat(a, b) => format!("{}{}", to_pattern(a), to_pattern(b)),
        Regex::Union(a, b) => format!("({}|{})", to_pattern(a), to_pattern(b)),
        Regex::Star(r) => match **r {
            Regex::Char(_) | Regex::Range(..) => format!("{}*", to_pattern(r)),
            _ => format!("({})*", to_pattern(r)),
        },
    }
}
