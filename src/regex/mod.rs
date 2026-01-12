//! Regex with Brzozowski derivatives.

mod dfa;
mod examples;
mod nfa;
mod ops;
mod parse;
mod valids;

#[cfg(test)]
mod test;

pub use examples::{example, examples};
pub use ops::{
    deriv, derivative, is_empty, is_nullable, match_len, matches, prefix_match, product,
    product_traced, simplify, to_pattern, unwrap_star,
};
use std::collections::HashSet;
pub use valids::{cache_stats, clear_cache};
// Aliases for backwards compatibility
pub use valids::{cache_stats as valids_cache_stats, clear_cache as clear_valids_cache};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Regex {
    Empty,   // ∅
    Epsilon, // ε
    Char(char),
    Range(char, char),
    Concat(Box<Regex>, Box<Regex>),
    Union(Box<Regex>, Box<Regex>),
    Star(Box<Regex>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrefixStatus {
    Extensible(Regex),
    Complete,
    Prefix(Regex),
    NoMatch,
}

impl Regex {
    // ===== Construction =====
    pub fn literal(s: &str) -> Self {
        s.chars()
            .map(Self::Char)
            .reduce(Self::cat)
            .unwrap_or(Self::Epsilon)
    }
    pub fn cat(a: Self, b: Self) -> Self {
        Self::Concat(Box::new(a), Box::new(b))
    }
    pub fn or(a: Self, b: Self) -> Self {
        Self::Union(Box::new(a), Box::new(b))
    }
    pub fn star(r: Self) -> Self {
        Self::Star(Box::new(r))
    }
    pub fn plus(r: Self) -> Self {
        Self::cat(r.clone(), Self::star(r))
    }
    pub fn opt(r: Self) -> Self {
        Self::or(Self::Epsilon, r)
    }
    pub fn range(lo: char, hi: char) -> Self {
        Self::Range(lo, hi)
    }
    pub fn concat_many<I: IntoIterator<Item = Self>>(rs: I) -> Self {
        rs.into_iter().reduce(Self::cat).unwrap_or(Self::Epsilon)
    }
    pub fn union_many<I: IntoIterator<Item = Self>>(rs: I) -> Self {
        rs.into_iter().reduce(Self::or).unwrap_or(Self::Empty)
    }
    pub fn any_of(s: &str) -> Self {
        s.chars()
            .map(Self::Char)
            .reduce(Self::or)
            .unwrap_or(Self::Empty)
    }

    // ===== Predicates =====
    pub fn is_empty(&self) -> bool {
        ops::is_empty(self)
    }

    pub fn is_nullable(&self) -> bool {
        ops::is_nullable(self)
    }

    pub fn equiv(&self, other: &Self) -> bool {
        self.simplify() == other.simplify()
    }

    // ===== Simplification =====
    pub fn simplify(&self) -> Self {
        ops::simplify(self)
    }

    // ===== Derivatives =====
    pub fn deriv(&self, c: char) -> Self {
        ops::deriv(self, c)
    }

    pub fn derivative(&self, s: &str) -> Self {
        ops::derivative(self, s)
    }
    pub fn matches(&self, s: &str) -> bool {
        ops::matches(self, s)
    }

    pub fn match_len(&self, s: &str) -> Option<usize> {
        ops::match_len(self, s)
    }

    pub fn prefix_match(&self, s: &str) -> PrefixStatus {
        ops::prefix_match(self, s)
    }

    // ===== Cartesian Product =====
    /// Compute cartesian product: [[a,b], [x,y]] -> [ax, ay, bx, by]
    pub fn product(choices: Vec<Vec<Self>>) -> Vec<Self> {
        ops::product(choices)
    }

    /// Product with index tracking: returns (alt_indices, regex)
    pub fn product_traced(choices: Vec<Vec<Self>>) -> Vec<(Vec<usize>, Self)> {
        ops::product_traced(choices)
    }

    // ===== String Generation =====
    pub fn valids(&self, max: usize) -> HashSet<String> {
        valids::valids(self, max)
    }
    pub fn example(&self) -> Option<String> {
        examples::example(self)
    }
    pub fn examples(&self, n: usize) -> Vec<String> {
        examples::examples(self, n)
    }

    // dfa based ops
    pub fn has_intersection(&self, other: &Self) -> bool {
        let dfa = dfa::DFA::from_regex(self.clone());
        let other_dfa = dfa::DFA::from_regex(other.clone());
        let inter_dfa = &dfa & &other_dfa;
        inter_dfa.is_accepting()
    }

    // ===== Utilities =====
    pub fn unwrap_star(&self) -> Self {
        ops::unwrap_star(self)
    }

    pub fn to_pattern(&self) -> String {
        ops::to_pattern(self)
    }

    // ===== Common Patterns =====
    pub fn digit() -> Self {
        Self::Range('0', '9')
    }
    pub fn lower() -> Self {
        Self::Range('a', 'z')
    }
    pub fn upper() -> Self {
        Self::Range('A', 'Z')
    }
    pub fn alpha() -> Self {
        Self::or(Self::lower(), Self::upper())
    }
    pub fn alnum() -> Self {
        Self::or(Self::alpha(), Self::digit())
    }
    pub fn word() -> Self {
        Self::or(Self::alnum(), Self::Char('_'))
    }
}

impl std::fmt::Display for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Regex::Empty => write!(f, "∅"),
            Regex::Epsilon => write!(f, "ε"),
            Regex::Char(c) => write!(f, "{}", c),
            Regex::Range(start, end) => write!(f, "[{}-{}]", start, end),
            Regex::Concat(a, b) => write!(f, "{}{}", a, b),
            Regex::Union(a, b) => write!(f, "({}|{})", a, b),
            Regex::Star(r) => write!(f, "{}*", r),
        }
    }
}
