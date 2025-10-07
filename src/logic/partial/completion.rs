use super::*;
use crate::debug_trace;
use crate::logic::ast::SourceSpan;
use crate::logic::bind::partial::conclude_type_with_rule;
use crate::logic::grammar::{Grammar, Production, Symbol};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

// === Public completion API ================================================================== //

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompletionToken {
    Literal(String),
    Regex(String),
}

impl CompletionToken {
    fn literal<S: Into<String>>(text: S) -> Self {
        CompletionToken::Literal(text.into())
    }
    fn regex<S: Into<String>>(pattern: S) -> Self {
        CompletionToken::Regex(pattern.into())
    }
}

impl Ord for CompletionToken {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (CompletionToken::Literal(a), CompletionToken::Literal(b)) => a.cmp(b),
            (CompletionToken::Literal(_), CompletionToken::Regex(_)) => Ordering::Less,
            (CompletionToken::Regex(_), CompletionToken::Literal(_)) => Ordering::Greater,
            (CompletionToken::Regex(a), CompletionToken::Regex(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for CompletionToken {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompletionOrigin {
    PartialSymbol,
    NextSymbol,
    TailRepetition,
    AlternateProduction,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CompletionMetadata {
    pub branch: String,
    pub rule_name: Option<String>,
    pub type_hint: Option<String>,
    pub symbol_index: usize,
    pub origin: CompletionOrigin,
}

#[derive(Clone, Debug)]
pub struct CompletionCandidate {
    pub token: CompletionToken,
    pub span: SourceSpan,
    pub metadata: CompletionMetadata,
}

#[derive(Clone, Debug)]
pub struct CompletionSet {
    pub grammar: Grammar,
    pub candidates: Vec<CompletionCandidate>,
}

impl CompletionSet {
    fn new(grammar: Grammar, mut candidates: Vec<CompletionCandidate>) -> Self {
        let mut set = Self {
            grammar,
            candidates: Vec::new(),
        };
        set.candidates = candidates.drain(..).collect();
        set.dedup();
        set
    }

    pub fn iter(&self) -> impl Iterator<Item = &CompletionCandidate> {
        self.candidates.iter()
    }

    pub fn contains_literal<S: AsRef<str>>(&self, text: S) -> bool {
        let text = text.as_ref();
        self.candidates
            .iter()
            .any(|c| matches!(&c.token, CompletionToken::Literal(lit) if lit == text))
    }

    pub fn contains_regex<S: AsRef<str>>(&self, pattern: S) -> bool {
        let pattern = pattern.as_ref();
        self.candidates
            .iter()
            .any(|c| matches!(&c.token, CompletionToken::Regex(re) if re == pattern))
    }

    fn dedup(&mut self) {
        let mut best_by_token: HashMap<CompletionToken, CompletionCandidate> = HashMap::new();

        for candidate in self.candidates.drain(..) {
            let token_key = candidate.token.clone();
            let entry = best_by_token
                .entry(token_key)
                .or_insert_with(|| candidate.clone());

            if Self::prefer_replacement(entry, &candidate) {
                *entry = candidate;
                continue;
            }

            if Self::score(entry) == Self::score(&candidate) {
                if candidate.metadata.origin == CompletionOrigin::PartialSymbol
                    && entry.metadata.origin != CompletionOrigin::PartialSymbol
                {
                    *entry = candidate;
                    continue;
                }

                if candidate.metadata.origin == CompletionOrigin::PartialSymbol
                    && entry.metadata.origin == CompletionOrigin::PartialSymbol
                    && candidate.metadata.branch == "TypedParam"
                    && entry.metadata.branch != "TypedParam"
                {
                    *entry = candidate;
                }
            }
        }

        self.candidates = best_by_token.into_values().collect();
        self.candidates.sort_by(|a, b| a.token.cmp(&b.token));
    }

    fn prefer_replacement(current: &CompletionCandidate, candidate: &CompletionCandidate) -> bool {
        let current_score = Self::score(current);
        let candidate_score = Self::score(candidate);
        candidate_score < current_score
    }

    fn score(candidate: &CompletionCandidate) -> (u8, u8, usize, bool) {
        let origin_rank = match candidate.metadata.origin {
            CompletionOrigin::PartialSymbol => 0,
            CompletionOrigin::NextSymbol => 1,
            CompletionOrigin::TailRepetition => 2,
            CompletionOrigin::AlternateProduction => 3,
        };
        let type_hint_rank: u8 = if candidate.metadata.type_hint.is_some() {
            0
        } else {
            1
        };
        let rule_rank: u8 = if candidate.metadata.rule_name.is_some() {
            0
        } else {
            1
        };
        let has_branch_name = !candidate.metadata.branch.is_empty();
        (
            origin_rank,
            type_hint_rank.saturating_add(rule_rank),
            candidate.metadata.symbol_index,
            !has_branch_name,
        )
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CompletionOptions {
    pub max_suggestions: Option<usize>,
}

impl PartialAST {
    pub fn completions(&self, grammar: &Grammar, _k: usize) -> CompletionSet {
        debug_trace!(
            "partial.completion",
            "PartialAST::completions: input='{}'",
            self.input
        );
        let mut explorer =
            CompletionExplorer::new(grammar, self.input.len(), CompletionOptions::default());
        explorer.collect_from_node(self.root());
        let set = explorer.into_set();
        debug_trace!(
            "partial.completion",
            "PartialAST::completions: returning {} candidates",
            set.candidates.len()
        );
        set
    }
}

// === Internal helpers ======================================================================= //

struct CompletionExplorer<'g> {
    grammar: &'g Grammar,
    input_len: usize,
    options: CompletionOptions,
    first_sets: FirstSetResolver<'g>,
    candidates: Vec<CompletionCandidate>,
}

impl<'g> CompletionExplorer<'g> {
    fn new(grammar: &'g Grammar, input_len: usize, options: CompletionOptions) -> Self {
        Self {
            grammar,
            input_len,
            options,
            first_sets: FirstSetResolver::new(grammar),
            candidates: Vec::new(),
        }
    }

    fn into_set(self) -> CompletionSet {
        CompletionSet::new(self.grammar.clone(), self.candidates)
    }

    fn collect_from_node(&mut self, node: &PartialASTNode) {
        match node {
            PartialASTNode::Terminal(_) => {}
            PartialASTNode::NonTerminal(branches) => {
                if !branches.iter().any(|branch| branch.is_progressing()) {
                    self.collect_missing_productions(branches);
                }

                for branch in branches {
                    self.collect_from_branch(branch);
                }
            }
        }
    }

    fn collect_missing_productions(&mut self, branches: &[PartialNonTerminal]) {
        let Some(first_branch) = branches.first() else {
            return;
        };
        let nt_name = &first_branch.value;
        let Some(all_productions) = self.grammar.productions.get(nt_name) else {
            return;
        };

        'production: for production in all_productions {
            for branch in branches {
                if branch.production.production == *production {
                    continue 'production;
                }
            }
            self.collect_from_production(nt_name, production);
        }
    }

    fn collect_from_production(&mut self, nt_name: &str, production: &Production) {
        let tokens = self.first_sets.tokens_for_sequence(&production.rhs);
        if tokens.is_empty() {
            return;
        }

        let span = SourceSpan {
            start: self.input_len,
            end: self.input_len,
        };
        let metadata = CompletionMetadata {
            branch: nt_name.to_string(),
            rule_name: production.rule.clone(),
            type_hint: None,
            symbol_index: 0,
            origin: CompletionOrigin::AlternateProduction,
        };

        for token in tokens {
            if self.reached_limit() {
                break;
            }
            self.push_candidate(CompletionCandidate {
                token,
                span: span.clone(),
                metadata: metadata.clone(),
            });
        }
    }

    fn collect_from_branch(&mut self, branch: &PartialNonTerminal) {
        debug_trace!(
            "partial.completion",
            "CompletionExplorer::collect_from_branch: value='{}', production={:?}",
            branch.value,
            branch.production.production
        );

        if let Some(partial) = branch.production.partial_symbol_ref() {
            self.collect_from_partial_symbol(branch, partial);
            return;
        }

        if !branch.is_complete() {
            if let Some(index) = branch.production.next_symbol_index() {
                if let Some(symbol) = branch.production.symbol_at(index) {
                    self.collect_symbol_predictions(
                        branch,
                        index,
                        symbol,
                        CompletionOrigin::NextSymbol,
                    );
                }
            }
        } else {
            self.collect_tail_repetition(branch);
        }

        for child in &branch.children {
            self.collect_from_node(child);
        }
    }

    fn collect_from_partial_symbol(
        &mut self,
        branch: &PartialNonTerminal,
        partial: &PartialSymbol,
    ) {
        debug_trace!(
            "partial.completion",
            "CompletionExplorer::collect_from_partial_symbol: branch='{}', partial={:?}",
            branch.value,
            partial
        );

        match partial {
            PartialSymbol::Litteral {
                expected,
                byte_cursor,
                span,
                ..
            } => {
                let consumed = (*byte_cursor).min(expected.len());
                if consumed < expected.len() {
                    let remainder = expected[consumed..].to_string();
                    self.push_candidate(CompletionCandidate {
                        token: CompletionToken::literal(remainder),
                        span: span.clone(),
                        metadata: self.metadata_for_branch(
                            branch,
                            partial.symbol_index(),
                            CompletionOrigin::PartialSymbol,
                        ),
                    });
                } else {
                    self.collect_next_symbol_after(branch, partial.symbol_index());
                }
            }
            PartialSymbol::Regex { re, span, .. } => {
                self.push_candidate(CompletionCandidate {
                    token: CompletionToken::regex(re.as_str()),
                    span: span.clone(),
                    metadata: self.metadata_for_branch(
                        branch,
                        partial.symbol_index(),
                        CompletionOrigin::PartialSymbol,
                    ),
                });
            }
            _ => {
                if let Some(child) = branch.children.get(partial.symbol_index()) {
                    self.collect_from_node(child);
                }

                if branch.children.get(partial.symbol_index()).is_none() {
                    if let Some(symbol) = branch.production.symbol_at(partial.symbol_index()) {
                        self.collect_symbol_predictions(
                            branch,
                            partial.symbol_index(),
                            symbol,
                            CompletionOrigin::PartialSymbol,
                        );
                    }
                }
            }
        }
    }

    fn collect_next_symbol_after(&mut self, branch: &PartialNonTerminal, index: usize) {
        let next_index = index + 1;
        if let Some(symbol) = branch.production.symbol_at(next_index) {
            self.collect_symbol_predictions(
                branch,
                next_index,
                symbol,
                CompletionOrigin::NextSymbol,
            );
        } else if branch.is_complete() {
            self.collect_tail_repetition(branch);
        }
    }

    fn collect_symbol_predictions(
        &mut self,
        branch: &PartialNonTerminal,
        symbol_index: usize,
        symbol: &Symbol,
        origin: CompletionOrigin,
    ) {
        let tokens = self.first_sets.tokens_for_symbol(symbol);
        if tokens.is_empty() {
            if let Some(child) = branch.children.get(symbol_index) {
                self.collect_from_node(child);
            }
            return;
        }

        let metadata = self.metadata_for_branch(branch, symbol_index, origin);
        let span = SourceSpan {
            start: self.input_len,
            end: self.input_len,
        };
        for token in tokens {
            if self.reached_limit() {
                break;
            }
            self.push_candidate(CompletionCandidate {
                token,
                span: span.clone(),
                metadata: metadata.clone(),
            });
        }
    }

    fn collect_tail_repetition(&mut self, branch: &PartialNonTerminal) {
        // Repetition no longer supported
        let _ = branch;
    }

    fn reached_limit(&self) -> bool {
        match self.options.max_suggestions {
            Some(max) => self.candidates.len() >= max,
            None => false,
        }
    }

    fn push_candidate(&mut self, candidate: CompletionCandidate) {
        if self.reached_limit() {
            return;
        }
        self.candidates.push(candidate);
    }

    fn metadata_for_branch(
        &self,
        branch: &PartialNonTerminal,
        symbol_index: usize,
        origin: CompletionOrigin,
    ) -> CompletionMetadata {
        let rule_name = branch.production.rule_name().cloned();
        let type_hint = self.type_hint_for_branch(branch, rule_name.as_deref());
        CompletionMetadata {
            branch: branch.value.clone(),
            rule_name,
            type_hint,
            symbol_index,
            origin,
        }
    }

    fn type_hint_for_branch(
        &self,
        branch: &PartialNonTerminal,
        rule_name: Option<&str>,
    ) -> Option<String> {
        let Some(rule_name) = rule_name else {
            return None;
        };
        let rule = self.grammar.typing_rules.get(rule_name)?;
        conclude_type_with_rule(branch, rule).map(|ty| ty.to_string())
    }
}

// === FIRST set computation ================================================================== //

#[derive(Clone, Default)]
struct FirstSet {
    tokens: HashSet<CompletionToken>,
    nullable: bool,
}

impl FirstSet {
    fn with_token(token: CompletionToken) -> Self {
        let mut set = FirstSet::default();
        set.tokens.insert(token);
        set
    }

    fn merge(&mut self, other: &FirstSet) {
        self.tokens.extend(other.tokens.iter().cloned());
        if other.nullable {
            self.nullable = true;
        }
    }

    fn into_sorted_vec(self) -> Vec<CompletionToken> {
        let mut tokens: Vec<_> = self.tokens.into_iter().collect();
        tokens.sort();
        tokens
    }
}

struct FirstSetResolver<'g> {
    grammar: &'g Grammar,
    cache: HashMap<String, FirstSet>,
    visiting: HashSet<String>,
}

impl<'g> FirstSetResolver<'g> {
    fn new(grammar: &'g Grammar) -> Self {
        Self {
            grammar,
            cache: HashMap::new(),
            visiting: HashSet::new(),
        }
    }

    fn tokens_for_symbol(&mut self, symbol: &Symbol) -> Vec<CompletionToken> {
        self.first_for_symbol(symbol).into_sorted_vec()
    }

    fn tokens_for_sequence(&mut self, symbols: &[Symbol]) -> Vec<CompletionToken> {
        self.first_for_sequence(symbols).into_sorted_vec()
    }

    fn first_for_symbol(&mut self, symbol: &Symbol) -> FirstSet {
        match symbol {
            Symbol::Litteral(text) => FirstSet::with_token(CompletionToken::literal(text.clone())),
            Symbol::Regex(re) => FirstSet::with_token(CompletionToken::regex(re.as_str())),
            Symbol::Single {
                value, ..
            } => {
                self.first_for_symbol(value.as_ref())
            }
            Symbol::Group {
                symbols,
            } => {
                self.first_for_sequence(symbols)
            }
            Symbol::Expression(nt) => self.first_for_nonterminal(nt),
        }
    }

    fn first_for_sequence(&mut self, symbols: &[Symbol]) -> FirstSet {
        if symbols.is_empty() {
            let mut empty = FirstSet::default();
            empty.nullable = true;
            return empty;
        }

        let mut result = FirstSet::default();
        for (idx, symbol) in symbols.iter().enumerate() {
            let sym_first = self.first_for_symbol(symbol);
            result.merge(&sym_first);
            if !sym_first.nullable {
                return result;
            }
            if idx == symbols.len() - 1 {
                result.nullable = true;
            }
        }
        result
    }

    fn first_for_nonterminal(&mut self, nt: &str) -> FirstSet {
        if let Some(cached) = self.cache.get(nt) {
            return cached.clone();
        }

        if !self.visiting.insert(nt.to_string()) {
            return FirstSet::default();
        }

        let mut result = FirstSet::default();
        if let Some(productions) = self.grammar.productions.get(nt) {
            for production in productions {
                if production.rhs.is_empty() {
                    result.nullable = true;
                    continue;
                }
                let seq_first = self.first_for_sequence(&production.rhs);
                result.merge(&seq_first);
            }
        }

        self.visiting.remove(nt);
        self.cache.insert(nt.to_string(), result.clone());
        result
    }
}

#[test]
fn test_completions_xtlc() {
    let spec = crate::logic::tests::xtlc::xtlc_spec();
    let g = crate::logic::grammar::Grammar::load(spec.as_str()).unwrap();
    let mut p = PartialParser::new(g.clone());
    let past = p.partial("λx:").unwrap();
    dbg!(&past.root());
    let completions = past.completions(&g, 0);
    println!("Completions: {:#?}", completions);
    assert!(
        completions.contains_regex("[\\p{L}][\\p{L}\\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*"),
        "expected identifier regex in completions"
    );
}

#[test]
fn test_completions_clike() {
    let spec = crate::logic::tests::clike::c_like_spec();
    let g = crate::logic::grammar::Grammar::load(spec.as_str()).unwrap();
    let mut p = PartialParser::new(g.clone());
    let past = p.partial("int").unwrap();
    dbg!(&past.root());
    let completions = past.completions(&g, 0);
    println!("Completions: {:#?}", completions);
    assert!(
        completions.contains_regex("[a-zA-Z_][a-zA-Z0-9_]*"),
        "expected identifier regex in completions"
    );
}

// === Tests ================================================================================== //

#[cfg(test)]
mod tests {
    use crate::logic::partial::parse::PartialParser;
    use super::*;

    fn complete(spec: &str, input: &str) -> CompletionSet {
        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = PartialParser::new(g.clone());
        let past = p.partial(input).unwrap();
        past.completions(&g, 0)
    }

    #[test]
    fn test_completions() {
        let spec = r#"
        U ::= 'b' 'a' 'r' 'c' 'b' 'a' 'r' 'c' 'u'
        O ::= 'o'
        A ::= 'a'
        B ::= 'b' A 'r'
        start ::= U | (B 'c')* | 't'
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = PartialParser::new(g.clone());
        let input = "b a r c b a r c";
        let past = p.partial(input).unwrap();
        let completions = past.completions(&g, 0);
        println!("Completions: {:#?}", completions);

        assert!(
            completions.contains_literal("u"),
            "expected literal 'u' in completions"
        );
        assert!(
            completions.contains_literal("b"),
            "expected literal 'b' in completions"
        );
    }



    #[test]
    fn completion_first_sets_with_alternatives() {
        let spec = r#"
        A(ruleA) ::= 'a' 'x'?
        B(ruleB) ::= 'b'
        start ::= A | B
        "#;

        let completions = complete(spec, "");
        assert!(completions.contains_literal("a"), "expected 'a' from FIRST(start)");
        assert!(completions.contains_literal("b"), "expected 'b' from FIRST(start)");
    }

    #[test]
    fn completion_next_symbol_prediction() {
        let spec = r#"
        start ::= 'a' 'b'
        "#;
        let completions = complete(spec, "a");
        assert!(completions.contains_literal("b"), "expected next literal 'b'");
    }

    #[test]
    fn completion_tail_repetition_plus() {
        let spec = r#"
        start ::= 'a'+
        "#;
        let completions = complete(spec, "a");
        assert!(
            completions.contains_literal("a"),
            "expected tail repetition to suggest another 'a'"
        );
    }

    #[test]
    fn completion_nullable_group_lookahead() {
        let spec = r#"
        start ::= ('a')? 'b'
        "#;
        let completions = complete(spec, "");
        println!("{:#?}", completions);
        assert!(completions.contains_literal("a"), "nullable group allows 'a'");
        assert!(completions.contains_literal("b"), "nullable group allows lookahead 'b'");
    }

    #[test]
    fn completion_group_repetition_tail() {
        let spec = r#"
        start ::= ('a' 'b')* 'c'
        "#;
        let completions = complete(spec, "ab");
        // For group repetition, FIRST set for the group starts with 'a'
        assert!(
            completions.contains_literal("a"),
            "expected to suggest restarting the group with 'a'"
        );
    }

    #[test]
    fn completion_regex_identifier() {
        let spec = r#"
        Identifier ::= /[a-z][a-z0-9]*/
        start ::= Identifier
        "#;
        let completions = complete(spec, "");
        assert!(
            completions.contains_regex("[a-z][a-z0-9]*"),
            "expected identifier regex completion"
        );
    }

    #[test]
    fn completion_single_wrapped_regex() {
        let spec = r#"
        Identifier ::= /[A-Z][a-z]+/
        // Single wraps the inner regex (with a binding)
        Name(name) ::= Identifier[x]
        start ::= Name
        "#;
        let completions = complete(spec, "");
        assert!(
            completions.contains_regex("[A-Z][a-z]+"),
            "expected FIRST(Name) to expose inner Identifier regex"
        );
    }

    #[test]
    fn completion_deduplicates_identical_tokens() {
        let spec = r#"
        S1(r1) ::= 'x'
        S2(r2) ::= 'x' 'y'
        start ::= S1 | S2
        "#;
        let completions = complete(spec, "");
        // Only a single 'x' token should appear after dedup
        let count_x = completions
            .iter()
            .filter(|c| matches!(c.token, CompletionToken::Literal(ref s) if s == "x"))
            .count();
        assert_eq!(count_x, 1, "expected deduplication of identical 'x' suggestions");
    }

    #[test]
    fn completion_metadata_rule_and_type_hint() {
        let spec = r#"
        start(lit) ::= 'x'

        -------------- (lit)
        'void'
        "#;

        let completions = complete(spec, "");
        let cand = completions
            .iter()
            .find(|c| matches!(c.token, CompletionToken::Literal(ref s) if s == "x"))
            .expect("expected 'x' completion");

    assert_eq!(cand.metadata.rule_name.as_deref(), Some("lit"));
        // Type hint should be derivable from the rule with no premises
        assert!(
            cand.metadata
                .type_hint
                .as_deref()
                .map(|t| t.contains("void"))
                .unwrap_or(false),
            "expected type hint containing 'void'"
        );
    }
}
