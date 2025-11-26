use super::*;
use crate::debug_trace;
use crate::logic::grammar::{Grammar, Symbol};
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use std::collections::HashSet;

/// The result of computing valid next tokens for a partial parse.
#[derive(Clone, Debug)]
pub struct CompletionSet {
    /// The set of all valid next tokens (deduplicated)
    pub tokens: Vec<DerivativeRegex>,
}

impl CompletionSet {
    fn new(mut tokens: Vec<DerivativeRegex>) -> Self {
        // Deduplicate and sort
        let unique: HashSet<_> = tokens.drain(..).collect();
        let tokens: Vec<_> = unique.into_iter().collect();
        Self { tokens }
    }

    pub fn iter(&self) -> impl Iterator<Item = &DerivativeRegex> {
        self.tokens.iter()
    }

    pub fn matches(&self, text: &str) -> bool {
        let text = text.as_ref();
        self.tokens.iter().any(|t| match t.prefix_match(text) {
            PrefixStatus::Extensible(_) | PrefixStatus::Complete | PrefixStatus::Prefix(_) => true,
            PrefixStatus::NoMatch => match DerivativeRegex::from_str(text) {
                Ok(parsed) => &parsed == t,
                Err(_) => false,
            },
        })
    }
}

// === Implementation ========================================================================== //

impl PartialAST {
    /// Get all valid next tokens for this partial parse.
    pub fn completions(&self, grammar: &Grammar) -> CompletionSet {
        debug_trace!(
            "partial.completion",
            "PartialAST::completions: input='{}', roots={}",
            self.input,
            self.roots.len()
        );

        let mut tokens = Vec::new();
        for root in &self.roots {
            tokens.extend(root.collect_valid_tokens(grammar));
        }

        CompletionSet::new(tokens)
    }
}

impl NonTerminal {
    pub fn collect_valid_tokens(&self, grammar: &Grammar) -> Vec<DerivativeRegex> {
        let mut tokens = Vec::new();

        if self.is_complete() {
            // If complete, we can only extend the last token if it is extensible
            if let Some(last) = self.children.last() {
                tokens.extend(last.collect_extensions());
            }
            return tokens;
        }

        // Partial node: find the frontier
        if let Some(last_child) = self.children.last() {
            match last_child {
                Node::Terminal(Terminal::Partial {
                    remainder: Some(rem),
                    ..
                }) => {
                    tokens.push(rem.clone());
                }
                Node::NonTerminal(nt) => {
                    if !nt.is_complete() {
                        tokens.extend(nt.collect_valid_tokens(grammar));
                    } else {
                        // Last child is complete. We need the next symbol in the production.
                        let next_idx = self.children.len();
                        if let Some(symbol) = self.production.rhs.get(next_idx) {
                            tokens.extend(first_set(symbol, grammar));
                        }
                    }
                }
                Node::Terminal(Terminal::Complete { .. }) => {
                    // Last child is complete terminal. Next symbol.
                    let next_idx = self.children.len();
                    if let Some(symbol) = self.production.rhs.get(next_idx) {
                        tokens.extend(first_set(symbol, grammar));
                    }
                }
                _ => {}
            }
        } else {
            // No children. First symbol.
            if let Some(symbol) = self.production.rhs.first() {
                tokens.extend(first_set(symbol, grammar));
            }
        }

        tokens
    }
}

impl Node {
    fn collect_extensions(&self) -> Vec<DerivativeRegex> {
        match self {
            Node::Terminal(Terminal::Complete {
                extension: Some(ext),
                ..
            }) => vec![ext.clone()],
            Node::NonTerminal(nt) => {
                if let Some(last) = nt.children.last() {
                    last.collect_extensions()
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }
}

/// Get the FIRST set for a symbol (all tokens that can start this symbol).
fn first_set(symbol: &Symbol, grammar: &Grammar) -> Vec<DerivativeRegex> {
    first_set_rec(symbol, grammar, &mut HashSet::new())
}

fn first_set_rec(
    symbol: &Symbol,
    grammar: &Grammar,
    visited: &mut HashSet<String>,
) -> Vec<DerivativeRegex> {
    match symbol {
        Symbol::Regex { regex, .. } => vec![regex.clone()],
        Symbol::Expression { name: nt_name, .. } => {
            if visited.contains(nt_name) {
                return vec![];
            }
            visited.insert(nt_name.clone());

            let res = if let Some(productions) = grammar.productions.get(nt_name) {
                productions
                    .iter()
                    .flat_map(|prod| {
                        if let Some(first_sym) = prod.rhs.first() {
                            first_set_rec(first_sym, grammar, visited)
                        } else {
                            vec![]
                        }
                    })
                    .collect()
            } else {
                vec![]
            };

            visited.remove(nt_name);
            res
        }
    }
}

// === Tests ================================================================================== //

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::partial::parse::Parser;

    fn complete(spec: &str, input: &str) -> CompletionSet {
        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let past = p.partial(input).unwrap();
        past.completions(&g)
    }

    #[test]
    fn test_completions() {
        let spec = r#"
    U ::= 'b' 'a' 'r' 'c' 'b' 'a' 'r' 'c' 'u'
    A ::= 'a'
    B ::= 'b' A 'r'
    Loop ::= B 'c' Loop | B 'c'
    start ::= U | Loop | 't'
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let input = "b a r c b a r c";
        let past = p.partial(input).unwrap();

        println!("Partial AST roots: {}", past.roots.len());

        let completions = past.completions(&g);
        println!("Completions: {:?}", completions);

        assert!(
            completions.matches("u"),
            "expected literal 'u' in completions"
        );
        assert!(
            completions.matches("b"),
            "expected literal 'b' in completions"
        );
    }

    #[test]
    fn completion_first_sets_with_alternatives() {
        let spec = r#"
    A(ruleA) ::= 'a' 'x' | 'a'
        B(ruleB) ::= 'b'
        start ::= A | B
        "#;

        let completions = complete(spec, "");
        assert!(completions.matches("a"), "expected 'a' from FIRST(start)");
        assert!(completions.matches("b"), "expected 'b' from FIRST(start)");
    }

    #[test]
    fn completion_next_symbol_prediction() {
        let spec = r#"
        start ::= 'a' 'b'
        "#;
        let completions = complete(spec, "a");
        assert!(completions.matches("b"), "expected next literal 'b'");
    }

    #[test]
    fn completion_binary_op_requires_operand() {
        let spec = r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Literal ::= Number[n]
        Variable ::= Identifier[x]
        AtomicExpr ::= Literal | Variable | '(' Expression ')'
    Operator ::= '+' | '-' | '*' | '/'
    BinaryOp ::= AtomicExpr[left] Operator[op] AtomicExpr[right]
        Expression ::= AtomicExpr | BinaryOp
        "#;

        let completions = complete(spec, "");
        assert!(
            completions.matches("[0-9]+"),
            "expected numeric literal to be suggested before operators"
        );
        assert!(
            !completions.matches("+"),
            "operator '+' should not be suggested before first operand"
        );
    }

    #[test]
    fn completion_tail_repetition_plus() {
        let spec = r#"
    start ::= 'a' | 'a' start
        "#;
        let completions = complete(spec, "a");
        assert!(
            completions.matches("a"),
            "expected tail repetition to suggest another 'a'"
        );
    }

    #[test]
    fn completion_nullable_group_lookahead() {
        let spec = r#"
    start ::= 'a' 'b' | 'b'
        "#;
        let completions = complete(spec, "");
        assert!(completions.matches("a"), "nullable group allows 'a'");
        assert!(
            completions.matches("b"),
            "nullable group allows lookahead 'b'"
        );
    }

    #[test]
    fn completion_group_repetition_tail() {
        let spec = r#"
    start ::= 'c' | 'a' 'b' start
        "#;
        let completions = complete(spec, "ab");
        // For group repetition, FIRST set for the group starts with 'a'
        assert!(
            completions.matches("a"),
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
            completions.matches("[a-z][a-z0-9]*"),
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
            completions.matches("[A-Z][a-z]+"),
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
            .filter(|t| t.equiv(&DerivativeRegex::literal("x")))
            .count();
        assert_eq!(
            count_x, 1,
            "expected deduplication of identical 'x' suggestions"
        );
    }

    // ============================================================================
    // Some tests on edge cases, not full suit
    // ============================================================================

    #[test]
    fn completion_single_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let completions = complete(spec, "");
        // Single literal production
        assert!(completions.matches("hello"), "should suggest 'hello'");
        assert_eq!(
            completions.tokens.len(),
            1,
            "should have exactly one completion"
        );
    }

    #[test]
    fn completion_nested_nullable_groups() {
        let spec = r#"
    start ::= 'a' 'b' 'c' | 'a' 'c' | 'b' 'c' | 'c'
        "#;
        let completions = complete(spec, "");
        // Should suggest 'a', 'b', and 'c' (all three are valid starts)
        assert!(completions.matches("a"), "should suggest 'a'");
        assert!(completions.matches("b"), "should suggest 'b'");
        assert!(completions.matches("c"), "should suggest 'c'");
    }

    #[test]
    fn completion_group_with_optional_prefix() {
        let spec = r#"
    start ::= 'a' 'b' 'c' 'd' | 'c' 'd'
        "#;
        let completions = complete(spec, "");
        // Group is optional, so should suggest both 'a' (from group) and 'c' (skipping group)
        assert!(
            completions.matches("a"),
            "should suggest 'a' from optional group"
        );
        assert!(
            completions.matches("c"),
            "should suggest 'c' (skipping optional group)"
        );
    }

    #[test]
    fn completion_multiple_alternatives_all_contribute() {
        let spec = r#"
        A ::= 'x'
        B ::= 'y'
        C ::= 'z'
        start ::= A | B | C
        "#;
        let completions = complete(spec, "");
        // All three alternatives should contribute their FIRST sets
        assert!(completions.matches("x"), "should include 'x' from A");
        assert!(completions.matches("y"), "should include 'y' from B");
        assert!(completions.matches("z"), "should include 'z' from C");
        assert_eq!(
            completions.tokens.len(),
            3,
            "should have exactly 3 completions"
        );
    }

    #[test]
    fn completion_deeply_nested_nonterminals() {
        let spec = r#"
        D ::= 'd'
        C ::= D
        B ::= C
        A ::= B
        start ::= A
        "#;
        let completions = complete(spec, "");
        // Should drill down through all nonterminals to find 'd'
        assert!(
            completions.matches("d"),
            "should find 'd' through nested nonterminals"
        );
        assert_eq!(
            completions.tokens.len(),
            1,
            "should have exactly one completion"
        );
    }

    #[test]
    fn completion_partial_literal_midway() {
        let spec = r#"
        start ::= 'hello' 'world'
        "#;
        let completions = complete(spec, "hello");
        // After matching first literal, should suggest second
        assert!(
            completions.matches("world"),
            "should suggest 'world' after 'hello'"
        );
    }

    #[test]
    fn completion_star_repetition_can_skip() {
        let spec = r#"
    A ::= 'a'
    start ::= 'b' | A start
        "#;
        let completions = complete(spec, "");
        // * allows zero matches, so both 'a' and 'b' are valid
        assert!(
            completions.matches("a"),
            "should suggest 'a' from repetition"
        );
        assert!(
            completions.matches("b"),
            "should suggest 'b' since * is nullable"
        );
    }

    #[test]
    fn completion_plus_repetition_after_one_match() {
        let spec = r#"
    A ::= 'a'
    start ::= A 'b' | A start
        "#;
        let completions = complete(spec, "a");
        // After one match of A, can repeat or continue to 'b'
        assert!(completions.matches("a"), "can repeat A");
        assert!(completions.matches("b"), "can continue to 'b'");
    }

    #[test]
    fn completion_optional_single_symbol() {
        let spec = r#"
    Foo ::= 'foo'
    start ::= Foo 'bar' | 'bar'
        "#;
        let completions = complete(spec, "");
        // Optional nonterminal's FIRST and following symbol
        assert!(
            completions.matches("foo"),
            "should suggest 'foo' from optional Foo"
        );
        assert!(
            completions.matches("bar"),
            "should suggest 'bar' since Foo is optional"
        );
    }

    #[test]
    fn completion_regex_alternatives() {
        let spec = r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z]+/
        start ::= Number | Identifier
        "#;
        let completions = complete(spec, "");
        assert!(completions.matches("[0-9]+"), "should suggest number regex");
        assert!(
            completions.matches("[a-z]+"),
            "should suggest identifier regex"
        );
    }

    #[test]
    fn completion_mixed_literals_and_regex() {
        let spec = r#"
        Num ::= /[0-9]+/
        start ::= 'let' Num
        "#;
        let completions = complete(spec, "");
        assert!(completions.matches("let"), "should suggest 'let' first");
        // Note: may also suggest other tokens from alternative parses

        let completions = complete(spec, "let");
        assert!(
            completions.matches("[0-9]+"),
            "should suggest regex after 'let'"
        );
    }

    #[test]
    fn completion_multiple_completed_repetitions() {
        let spec = r#"
    ASeq ::= ε | 'a' ASeq
    BSeq ::= ε | 'b' BSeq
    start ::= ASeq BSeq 'c'
        "#;
        let completions = complete(spec, "aabb");
        // After matching 'aa' and 'bb', can continue second repetition or move to 'c'
        // Cannot continue first repetition because we have already started BSeq (seen 'b')
        assert!(
            !completions.matches("a"),
            "cannot continue first repetition after starting second"
        );
        assert!(completions.matches("b"), "can continue second repetition");
        assert!(completions.matches("c"), "can move to 'c'");
    }

    #[test]
    fn completion_group_with_multiple_symbols() {
        let spec = r#"
    start ::= 'a' 'b' 'c' Tail
    Tail ::= 'd' | 'a' 'b' 'c' Tail
        "#;
        let completions = complete(spec, "abc");
        // After one complete group iteration, can repeat or continue
        assert!(completions.matches("a"), "can repeat the group");
        assert!(completions.matches("d"), "can move to 'd'");
    }

    #[test]
    fn completion_no_ambiguity_in_sequence() {
        let spec = r#"
        start ::= 'a' 'b' 'c'
        "#;
        let completions = complete(spec, "ab");
        // Unambiguous: only 'c' is valid next
        assert!(completions.matches("c"), "should suggest 'c'");
        assert_eq!(
            completions.tokens.len(),
            1,
            "should have exactly one completion"
        );
    }

    #[test]
    fn completion_alternatives_with_common_prefix() {
        let spec = r#"
        A ::= 'a' 'b'
        B ::= 'a' 'c'
        start ::= A | B
        "#;
        let completions = complete(spec, "a");
        // After common prefix 'a', both alternatives are possible
        assert!(completions.matches("b"), "should suggest 'b' from A");
        assert!(completions.matches("c"), "should suggest 'c' from B");
    }

    // ============================================================================
    // Typed Completion Tests
    // ============================================================================

    #[test]
    fn typed_completion_basic() {
        let spec = r#"
        Num(num) ::= /[0-9]+/
        start ::= Num
        
        -------------- (num)
        'int'
        "#;

        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("").unwrap();
        let untyped = ast.completions(&g);
        assert!(untyped.matches("[0-9]+"));
    }

    #[test]
    fn typed_completion_with_context() {
        let spec = r#"
        Var(var) ::= /[a-z]+/
        Num(num) ::= /[0-9]+/
        Assign(assign) ::= Var '=' Num
        start ::= Assign
        
        -------------- (var)
        'string'
        
        -------------- (num)
        'int'
        
        -------------- (assign)
        'unit'
        "#;

        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());

        // At start, should suggest variable
        let ast1 = p.partial("").unwrap();
        let completions1 = ast1.completions(&g);
        assert!(
            completions1.matches("[a-z]+"),
            "should suggest var at start"
        );

        // After var and =, should suggest number
        let ast2 = p.partial("x =").unwrap();
        let completions2 = ast2.completions(&g);
        assert!(completions2.matches("[0-9]+"), "should suggest num after =");
    }

    #[test]
    fn typed_completion_preserves_all_valid() {
        let spec = r#"
        A(ruleA) ::= 'a'
        B(ruleB) ::= 'b'
        start ::= A | B
        
        -------------- (ruleA)
        'typeA'
        
        -------------- (ruleB)
        'typeB'
        "#;

        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("").unwrap();

        let typed = ast.completions(&g);

        // Both should be suggested (both have valid typing rules)
        assert!(typed.matches("a"), "should suggest 'a'");
        assert!(typed.matches("b"), "should suggest 'b'");
        assert_eq!(typed.tokens.len(), 2);
    }

    #[test]
    fn typed_completion_complex_expression() {
        let spec = r#"
        Num(num) ::= /[0-9]+/
        Add(add) ::= Num '+' Num
        start ::= Add
        
        -------------- (num)
        'int'
        
        -------------- (add)
        'int'
        "#;

        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());

        let ast = p.partial("42 +").unwrap();
        let completions = ast.completions(&g);

        // After '+', should suggest another number
        assert!(
            completions.matches("[0-9]+"),
            "should suggest number after +"
        );
    }

    #[test]
    fn test_paren_expr_is_complete() {
        let spec = r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        
        Literal(lit) ::= Number[n]
        Variable(var) ::= Identifier[x]
        
        AtomicExpr ::= Literal | Variable | '(' Expression ')'
    Operator ::= '+' | '-' | '*' | '/'
    BinaryOp(binop) ::= AtomicExpr[left] Operator[op] AtomicExpr[right]
        
        Expression ::= AtomicExpr | BinaryOp
        "#;

        let grammar = crate::logic::grammar::Grammar::load(spec).unwrap();

        crate::set_debug_level(crate::DebugLevel::Trace);
        crate::set_debug_input(Some("(42)".to_string()));

        let mut parser1 = crate::logic::Parser::new(grammar.clone());
        let partial1 = parser1.partial("42").unwrap();
        println!("=== Parsing '42' ===");
        println!("Complete: {}", partial1.complete());
        println!("Roots: {}", partial1.roots.len());

        // Second test: parse "(42" - this is where the issue is
        let mut parser2 = crate::logic::Parser::new(grammar.clone());
        let partial2 = parser2.partial("(42").unwrap();
        println!("\n=== Parsing '(42' ===");
        println!("Complete: {}", partial2.complete());
        println!("Roots: {}", partial2.roots.len());

        // Completions
        let completions = partial2.completions(&grammar);
        println!("Completions for '(42': {:?}", completions);
    }
}
