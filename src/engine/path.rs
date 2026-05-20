//! Formal path objects used across grammar analysis and typed derivations.

use std::ops::{Add, Mul};

/// Path in a realized or partial derivation tree.
/// Invariant: entries are child indices in tree order.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TreePath(Vec<usize>);

impl TreePath {
    #[must_use] 
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, index: usize) {
        self.0.push(index);
    }

    pub fn pop(&mut self) -> Option<usize> {
        self.0.pop()
    }

    #[must_use] 
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use] 
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[must_use] 
    pub fn as_slice(&self) -> &[usize] {
        &self.0
    }

    pub fn iter(&self) -> std::slice::Iter<'_, usize> {
        self.0.iter()
    }

    #[must_use] 
    pub fn into_vec(self) -> Vec<usize> {
        self.0
    }
}

impl From<Vec<usize>> for TreePath {
    fn from(path: Vec<usize>) -> Self {
        Self(path)
    }
}

impl AsRef<[usize]> for TreePath {
    fn as_ref(&self) -> &[usize] {
        self.as_slice()
    }
}

/// One grammar-space transition.
/// `i` selects a child index and `a` constrains the active alternative.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PathStep {
    pub i: usize,
    pub a: usize,
}

impl PathStep {
    #[must_use] 
    pub fn new(i: usize, a: usize) -> Self {
        Self { i, a }
    }
}

/// Path in grammar space.
/// Unlike `TreePath`, this records production-alternative constraints.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct GrammarPath {
    steps: Vec<PathStep>,
}

impl Ord for GrammarPath {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.steps.len().cmp(&other.steps.len()) {
            std::cmp::Ordering::Equal => self.steps.cmp(&other.steps),
            ord => ord,
        }
    }
}

impl PartialOrd for GrammarPath {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl GrammarPath {
    #[must_use] 
    pub fn new() -> Self {
        Self { steps: Vec::new() }
    }

    pub fn push(&mut self, step: PathStep) {
        self.steps.push(step);
    }

    pub fn pop(&mut self) -> Option<PathStep> {
        self.steps.pop()
    }

    #[must_use] 
    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    #[must_use] 
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    #[must_use] 
    pub fn steps(&self) -> &[PathStep] {
        &self.steps
    }

    pub fn iter(&self) -> std::slice::Iter<'_, PathStep> {
        self.steps.iter()
    }

    #[must_use] 
    pub fn forward(&self) -> Option<(PathStep, Self)> {
        let first = self.steps.first().cloned()?;
        let rest = self.steps[1..].to_vec();
        Some((first, Self { steps: rest }))
    }

    #[must_use] 
    pub fn alts(&self) -> Vec<usize> {
        self.steps.iter().map(|step| step.a).collect()
    }

    #[must_use] 
    pub fn indices(&self) -> Vec<usize> {
        self.steps.iter().map(|step| step.i).collect()
    }

    #[must_use] 
    pub fn idxs(&self) -> Vec<usize> {
        self.indices()
    }
}

impl Add for GrammarPath {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self {
        self.steps.extend(rhs.steps);
        self
    }
}

impl Mul<PathStep> for GrammarPath {
    type Output = Self;

    fn mul(mut self, rhs: PathStep) -> Self {
        self.steps.push(rhs);
        self
    }
}

impl From<Vec<PathStep>> for GrammarPath {
    fn from(steps: Vec<PathStep>) -> Self {
        Self { steps }
    }
}
