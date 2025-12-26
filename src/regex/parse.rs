//! Regex parsing from strings.

use super::Regex;
use regex_syntax::hir::{Class, HirKind};

impl Regex {
    pub fn new(pattern: &str) -> Result<Self, String> { Self::from_str(pattern) }
    
    pub fn from_str(pattern: &str) -> Result<Self, String> {
        regex_syntax::Parser::new().parse(pattern).map(|h| Self::from_hir(&h)).map_err(|e| e.to_string())
    }

    pub fn from_hir(hir: &regex_syntax::hir::Hir) -> Self {
        match hir.kind() {
            HirKind::Empty => Self::Epsilon,
            HirKind::Literal(l) => Self::literal(std::str::from_utf8(&l.0).unwrap_or("")),
            HirKind::Class(Class::Unicode(c)) => c.iter()
                .map(|r| if r.start() == r.end() { Self::Char(r.start()) } else { Self::Range(r.start(), r.end()) })
                .reduce(Self::or).unwrap_or(Self::Empty),
            HirKind::Class(Class::Bytes(c)) => c.iter()
                .map(|r| if r.start() == r.end() { Self::Char(r.start() as char) } else { Self::Range(r.start() as char, r.end() as char) })
                .reduce(Self::or).unwrap_or(Self::Empty),
            HirKind::Look(_) => Self::Epsilon,
            HirKind::Repetition(rep) => {
                let inner = Self::from_hir(&rep.sub);
                match (rep.min, rep.max) {
                    (0, Some(1)) => Self::opt(inner),
                    (0, None) => Self::star(inner),
                    (1, None) => Self::plus(inner),
                    (n, None) => Self::cat(Self::repeat(inner.clone(), n as usize), Self::star(inner)),
                    (n, Some(m)) if n == m => Self::repeat(inner, n as usize),
                    (n, Some(m)) => Self::cat(Self::repeat(inner.clone(), n as usize), Self::repeat_opt(inner, (m - n) as usize)),
                }
            }
            HirKind::Concat(xs) => xs.iter().map(Self::from_hir).reduce(Self::cat).unwrap_or(Self::Epsilon),
            HirKind::Alternation(xs) => xs.iter().map(Self::from_hir).reduce(Self::or).unwrap_or(Self::Empty),
            HirKind::Capture(cap) => Self::from_hir(&cap.sub),
        }
    }

    fn repeat(r: Self, n: usize) -> Self { (0..n).fold(Self::Epsilon, |acc, _| Self::cat(acc, r.clone())) }
    fn repeat_opt(r: Self, n: usize) -> Self { (0..n).fold(Self::Epsilon, |acc, _| Self::cat(acc, Self::opt(r.clone()))) }
}


