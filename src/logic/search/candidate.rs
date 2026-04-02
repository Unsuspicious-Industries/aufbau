use crate::logic::fusion::ast::FusionForest;
use crate::logic::grammar::Grammar;
use crate::logic::typing::{Context, gather_raw_types};
use crate::regex::Regex as DerivativeRegex;
use std::collections::HashSet;

/// Candidate gathering strategy
pub(crate) trait Strategy {
    fn gather(&self, token: &DerivativeRegex, ctx: &Ctx<'_>) -> Vec<String>;
}

pub(crate) struct Ctx<'a> {
    pub grammar: &'a Grammar,
    pub ast: &'a FusionForest<'a>,
    pub ctx: &'a Context,
    pub seeds: &'a [String],
}

/// Composite strategy combining multiple sources
pub(crate) struct Composite {
    sources: Vec<Box<dyn Source>>,
}

impl Composite {
    pub fn new(sources: Vec<Box<dyn Source>>) -> Self {
        Self { sources }
    }

    pub fn default() -> Self {
        Self::new(vec![
            Box::new(Example),
            Box::new(Bindings),
            Box::new(BoundTexts),
            Box::new(Completions),
            Box::new(Seeds),
        ])
    }
}

impl Strategy for Composite {
    fn gather(&self, token: &DerivativeRegex, ctx: &Ctx<'_>) -> Vec<String> {
        let example = token.example();
        let allowed = |s: &str| {
            !ctx.grammar.special_tokens.iter().any(|t| t == s) || example.as_deref() == Some(s)
        };

        self.sources
            .iter()
            .flat_map(|src| src.provide(token, ctx))
            .filter(|s| allowed(s) && token.matches(s))
            .collect::<HashSet<_>>()
            .into_iter()
            .collect()
    }
}

/// Individual candidate source
pub(crate) trait Source {
    fn provide(&self, token: &DerivativeRegex, ctx: &Ctx<'_>) -> Vec<String>;
}

pub(crate) struct Example;
impl Source for Example {
    fn provide(&self, token: &DerivativeRegex, _: &Ctx<'_>) -> Vec<String> {
        token.example().into_iter().collect()
    }
}

pub(crate) struct Bindings;
impl Source for Bindings {
    fn provide(&self, _: &DerivativeRegex, ctx: &Ctx<'_>) -> Vec<String> {
        ctx.ctx.bindings.keys().cloned().collect()
    }
}

pub(crate) struct BoundTexts;
impl Source for BoundTexts {
    fn provide(&self, _: &DerivativeRegex, ctx: &Ctx<'_>) -> Vec<String> {
        ctx.ast.bound_texts()
    }
}

pub(crate) struct Completions;
impl Source for Completions {
    fn provide(&self, _: &DerivativeRegex, ctx: &Ctx<'_>) -> Vec<String> {
        ctx.ast
            .completions(ctx.grammar)
            .iter()
            .filter_map(|t| t.example())
            .collect()
    }
}

pub(crate) struct Seeds;
impl Source for Seeds {
    fn provide(&self, _: &DerivativeRegex, ctx: &Ctx<'_>) -> Vec<String> {
        ctx.seeds.to_vec()
    }
}

pub(crate) fn collect_seeds(grammar: &Grammar) -> Vec<String> {
    let mut seen = HashSet::new();
    gather_raw_types(grammar)
        .into_iter()
        .chain(["a", "x", "0", "1"].iter().map(|s| s.to_string()))
        .filter(|s| seen.insert(s.clone()))
        .collect()
}
