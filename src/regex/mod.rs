//! Regex with Brzozowski derivatives.

mod parse;
mod valids;

#[cfg(test)]
mod test;

use std::collections::HashSet;
pub use valids::{clear_cache, cache_stats};
// Aliases for backwards compatibility
pub use valids::{clear_cache as clear_valids_cache, cache_stats as valids_cache_stats};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Regex {
    Empty,                          // ∅
    Epsilon,                        // ε  
    Char(char),
    Range(char, char),
    Concat(Box<Regex>, Box<Regex>),
    Union(Box<Regex>, Box<Regex>),
    Star(Box<Regex>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrefixStatus { Extensible(Regex), Complete, Prefix(Regex), NoMatch }

impl Regex {
    // ===== Construction =====
    pub fn literal(s: &str) -> Self { s.chars().map(Self::Char).reduce(Self::cat).unwrap_or(Self::Epsilon) }
    pub fn cat(a: Self, b: Self) -> Self { Self::Concat(Box::new(a), Box::new(b)) }
    pub fn or(a: Self, b: Self) -> Self { Self::Union(Box::new(a), Box::new(b)) }
    pub fn star(r: Self) -> Self { Self::Star(Box::new(r)) }
    pub fn plus(r: Self) -> Self { Self::cat(r.clone(), Self::star(r)) }
    pub fn opt(r: Self) -> Self { Self::or(Self::Epsilon, r) }
    pub fn range(lo: char, hi: char) -> Self { Self::Range(lo, hi) }
    pub fn concat_many<I: IntoIterator<Item=Self>>(rs: I) -> Self { rs.into_iter().reduce(Self::cat).unwrap_or(Self::Epsilon) }
    pub fn union_many<I: IntoIterator<Item=Self>>(rs: I) -> Self { rs.into_iter().reduce(Self::or).unwrap_or(Self::Empty) }
    pub fn any_of(s: &str) -> Self { s.chars().map(Self::Char).reduce(Self::or).unwrap_or(Self::Empty) }

    // ===== Predicates =====
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Empty => true,
            Self::Concat(a, b) => a.is_empty() || b.is_empty(),
            Self::Union(a, b) => a.is_empty() && b.is_empty(),
            _ => false,
        }
    }
    
    pub fn is_nullable(&self) -> bool {
        match self {
            Self::Epsilon | Self::Star(_) => true,
            Self::Concat(a, b) => a.is_nullable() && b.is_nullable(),
            Self::Union(a, b) => a.is_nullable() || b.is_nullable(),
            _ => false,
        }
    }

    pub fn equiv(&self, other: &Self) -> bool { self.simplify() == other.simplify() }

    // ===== Simplification =====
    pub fn simplify(&self) -> Self {
        match self {
            Self::Concat(a, b) => match (a.simplify(), b.simplify()) {
                (Self::Empty, _) | (_, Self::Empty) => Self::Empty,
                (Self::Epsilon, s) | (s, Self::Epsilon) => s,
                (a, b) => Self::cat(a, b),
            },
            Self::Union(a, b) => match (a.simplify(), b.simplify()) {
                (Self::Empty, s) | (s, Self::Empty) => s,
                (a, b) if a == b => a,
                (a, b) => Self::or(a, b),
            },
            Self::Star(r) => match r.simplify() {
                Self::Empty | Self::Epsilon => Self::Epsilon,
                s => Self::star(s),
            },
            r => r.clone(),
        }
    }

    // ===== Derivatives =====
    pub fn deriv(&self, c: char) -> Self {
        match self {
            Self::Empty | Self::Epsilon => Self::Empty,
            Self::Char(ch) => if *ch == c { Self::Epsilon } else { Self::Empty },
            Self::Range(lo, hi) => if c >= *lo && c <= *hi { Self::Epsilon } else { Self::Empty },
            Self::Union(a, b) => Self::or(a.deriv(c), b.deriv(c)),
            Self::Concat(a, b) if a.is_nullable() => Self::or(Self::cat(a.deriv(c), (**b).clone()), b.deriv(c)),
            Self::Concat(a, b) => Self::cat(a.deriv(c), (**b).clone()),
            Self::Star(r) => Self::cat(r.deriv(c), self.clone()),
        }
    }

    pub fn derivative(&self, s: &str) -> Self { s.chars().fold(self.clone(), |r, c| r.deriv(c)) }
    pub fn matches(&self, s: &str) -> bool { self.derivative(s).simplify().is_nullable() }

    pub fn prefix_match(&self, s: &str) -> PrefixStatus {
        let d = self.derivative(s).simplify();
        if d.is_empty() { PrefixStatus::NoMatch }
        else if matches!(d, Self::Empty) { PrefixStatus::Complete }
        else if d.is_nullable() { PrefixStatus::Extensible(d) }
        else { PrefixStatus::Prefix(d) }
    }

    // ===== Cartesian Product =====
    /// Compute cartesian product: [[a,b], [x,y]] -> [ax, ay, bx, by]
    pub fn product(choices: Vec<Vec<Self>>) -> Vec<Self> {
        choices.into_iter().fold(vec![Self::Epsilon], |acc, alts| {
            acc.into_iter().flat_map(|p| alts.iter().map(move |r| Self::cat(p.clone(), r.clone()))).collect()
        })
    }

    /// Product with index tracking: returns (alt_indices, regex)
    pub fn product_traced(choices: Vec<Vec<Self>>) -> Vec<(Vec<usize>, Self)> {
        choices.into_iter().fold(vec![(vec![], Self::Epsilon)], |acc, alts| {
            acc.into_iter().flat_map(|(idx, p)| {
                alts.iter().enumerate().map(move |(i, r)| {
                    let mut v = idx.clone(); v.push(i); (v, Self::cat(p.clone(), r.clone()))
                })
            }).collect()
        })
    }

    // ===== String Generation =====
    pub fn valids(&self, max: usize) -> HashSet<String> { valids::valids(self, max) }
    pub fn example(&self) -> Option<String> { valids::example(self) }
    pub fn examples(&self, n: usize) -> Vec<String> { valids::examples(self, n) }

    // ===== Utilities =====
    pub fn unwrap_star(&self) -> Self { if let Self::Star(r) = self { (**r).clone() } else { self.clone() } }

    pub fn to_pattern(&self) -> String {
        match self {
            Self::Empty => "(?!)".into(),
            Self::Epsilon => String::new(),
            Self::Char(c) => if ".*+?|()[]{}\\^$".contains(*c) { format!("\\{c}") } else { c.to_string() },
            Self::Range(a, b) if a == b => Self::Char(*a).to_pattern(),
            Self::Range(a, b) => format!("[{a}-{b}]"),
            Self::Concat(a, b) => format!("{}{}", a.to_pattern(), b.to_pattern()),
            Self::Union(a, b) => format!("({}|{})", a.to_pattern(), b.to_pattern()),
            Self::Star(r) => match **r {
                Self::Char(_) | Self::Range(..) => format!("{}*", r.to_pattern()),
                _ => format!("({})*", r.to_pattern()),
            },
        }
    }

    // ===== Common Patterns =====
    pub fn digit() -> Self { Self::Range('0', '9') }
    pub fn lower() -> Self { Self::Range('a', 'z') }
    pub fn upper() -> Self { Self::Range('A', 'Z') }
    pub fn alpha() -> Self { Self::or(Self::lower(), Self::upper()) }
    pub fn alnum() -> Self { Self::or(Self::alpha(), Self::digit()) }
    pub fn word() -> Self { Self::or(Self::alnum(), Self::Char('_')) }
}
