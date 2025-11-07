use super::*;
use crate::debug_trace;
use crate::logic::grammar::{Grammar, RepetitionKind, Symbol};
use std::collections::HashSet;
use std::path::Prefix;
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};


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
        let mut tokens: Vec<_> = unique.into_iter().collect();
        Self { tokens }
    }

    pub fn iter(&self) -> impl Iterator<Item = &DerivativeRegex> {
        self.tokens.iter()
    }

    pub fn matches(&self, text: &str) -> bool {
        let text = text.as_ref();
        self.tokens
            .iter()
            .any(|t| match t.prefix_match(text) {
                PrefixStatus::Extensible(_) | PrefixStatus::Complete | PrefixStatus::Prefix(_) => true,
                _ => false,
            })
    }
}

// === Implementation ========================================================================== //

impl PartialAST {
    /// Get all valid next tokens for this partial parse.
    pub fn completions(&self, grammar: &Grammar) -> CompletionSet {
        debug_trace!(
            "partial.completion",
            "PartialAST::completions: input='{}'",
            self.input
        );
        let tokens = self.root().collect_valid_tokens(grammar);
        CompletionSet::new(tokens)
    }

}

impl NonTerminal {
    /// Collect valid next tokens from all alternatives.
    /// 
    /// Key invariant: If any alternative is complete, we should NOT suggest tokens
    /// from incomplete alternatives. This prevents suggesting tokens that would lead
    /// to dead-end states (violating the core theorem that incomplete ASTs must have
    /// valid completions).
    fn collect_valid_tokens(&self, grammar: &Grammar) -> Vec<DerivativeRegex> {
        debug_trace!(
            "partial.completion",
            "NonTerminal::collect_valid_tokens: nt='{}', alts={}, has_complete={}",
            self.name,
            self.alts.len(),
            self.is_complete()
        );

        // If we have at least one complete alternative, only collect from complete ones
        // (which will return empty unless they have tail repetitions)
        if self.is_complete() {
            debug_trace!(
                "partial.completion",
                "  Has complete alternative - only collecting from complete alts"
            );
            self.alts
                .iter()
                .filter(|alt| alt.is_complete())
                .flat_map(|alt| alt.collect_valid_tokens(grammar))
                .collect()
        } else {
            // No complete alternatives - collect from all
            self.alts
                .iter()
                .flat_map(|alt| alt.collect_valid_tokens(grammar))
                .collect()
        }
    }
}

impl Alt {
    /// Collect valid next tokens for this alternative.
    fn collect_valid_tokens(&self, grammar: &Grammar) -> Vec<DerivativeRegex> {
        debug_trace!(
            "partial.completion",
            "Alt::collect_valid_tokens: is_complete={}, cursor={}",
            self.is_complete(),
            self.cursor()
        );

        let mut tokens = Vec::new();
        
        // If complete, check for tail repetition
        if self.is_complete() {
            if let Some(last_symbol) = self.production.rhs.last() {
                match last_symbol.repetition() {
                    Some(RepetitionKind::OneOrMore | RepetitionKind::ZeroOrMore) => {
                        // Tail repetition: can repeat the last symbol
                        tokens.extend(first_set(last_symbol, grammar));
                    }
                    _ => {}
                }
            }
            return tokens;
        }

        // Not complete: get the cursor and find next valid tokens
        let cursor = self.cursor();
        
        // Special case: check if there are completed repeatable symbols before the cursor
        // These can be repeated even though we've moved past them
        for idx in 0..cursor {
            if let Some(symbol) = self.production.rhs.get(idx) {
                if matches!(symbol.repetition(), Some(RepetitionKind::ZeroOrMore | RepetitionKind::OneOrMore)) {
                    // This symbol can repeat, add its FIRST set
                    if self.symbol_complete(idx) {
                        tokens.extend(first_set(symbol, grammar));
                    }
                }
            }
        }
        
        // Check if we have a partial symbol in progress
        let mut cursor_symbol_is_partial = false;
        if let Some(slot) = self.slots.get(&cursor) {
            if let Slot::Partial { partial_symbol, node,.. } = slot {
                cursor_symbol_is_partial = true;
                // If we have a partial node, get completions from it
                if let Some(ParsedNode::NonTerminal(nt)) = node {
                    let nt_tokens = nt.collect_valid_tokens(grammar);
                    tokens.extend(nt_tokens);
                } else {
                    // No node yet, use the partial symbol's completions
                    let partial_tokens = completion_for_partial_symbol(partial_symbol, grammar);
                    tokens.extend(partial_tokens);
                }
            }
        }
        
        // Include nullable symbols before cursor
        for idx in 0..cursor {
            if let Some(symbol) = self.production.rhs.get(idx) {
                if grammar.symbol_nullable(symbol) && self.symbol_complete(idx) {
                    let sym_tokens = first_set(symbol, grammar);
                    tokens.extend(sym_tokens);
                }
            }
        }
        
        // If the cursor symbol is currently partial, we must finish it before
        // moving on to later symbols. The partial node's completions (and any
        // repeatable/nullable prefixes above) already cover valid suggestions.
        if cursor_symbol_is_partial {
            return tokens;
        }
        
        // Include symbols from cursor onward
        for idx in cursor..self.production.rhs.len() {
            if let Some(symbol) = self.production.rhs.get(idx) {
                let sym_tokens = first_set(symbol, grammar);
                tokens.extend(sym_tokens);
                
                // If this symbol is not nullable, stop here
                if !grammar.symbol_nullable( symbol) {
                    break;
                }
                // Otherwise, continue to next symbol (nullable symbols allow lookahead)
            }
        }
        
        tokens
    }
}

/// Get the FIRST set for a symbol (all tokens that can start this symbol).
fn first_set(symbol: &Symbol, grammar: &Grammar) -> Vec<DerivativeRegex> {
    match symbol {
        Symbol::Litteral(text) => vec![DerivativeRegex::literal(&text)],
        Symbol::Regex(re) => vec![re.clone()],

        Symbol::Expression(nt_name) => {
            // Look up nonterminal in grammar and get FIRST set of all its productions
            if let Some(productions) = grammar.productions.get(nt_name) {
                productions
                    .iter()
                    .flat_map(|prod| {
                        if let Some(first_sym) = prod.rhs.first() {
                            first_set(first_sym, grammar)
                        } else {
                            vec![]
                        }
                    })
                    .collect()
            } else {
                vec![]
            }
        }
        
        Symbol::Single { value, .. } => {
            // Single just wraps another symbol
            first_set(value, grammar)
        }
        
        Symbol::Group { symbols, .. } => {
            // Group's FIRST set is the FIRST of its first symbol
            // (with nullable handling for subsequent symbols if needed)
            let mut tokens = Vec::new();
            for sym in symbols {
                tokens.extend(first_set(sym, grammar));
                if grammar.symbol_nullable(sym) {
                    break;
                }
            }
            tokens
        }
    }
}



/// Get valid completions for a partial symbol in progress.
fn completion_for_partial_symbol(partial_symbol: &PartialSymbol, grammar: &Grammar) -> Vec<DerivativeRegex> {
    match partial_symbol {
        PartialSymbol::Terminal { derivative, .. } => {
            vec![derivative.clone()]
        }
        
        PartialSymbol::NonTerminal { nt, .. } => {
            // Partial nonterminal: get FIRST set
            // NOTE: This is only for when we haven't started parsing the nonterminal yet
            // If we have a partial node, the caller should use that node's completions instead
            if let Some(productions) = grammar.productions.get(nt) {
                productions
                    .iter()
                    .flat_map(|prod| {
                        if let Some(first_sym) = prod.rhs.first() {
                            first_set(first_sym, grammar)
                        } else {
                            vec![]
                        }
                    })
                    .collect()
            } else {
                vec![]
            }
        }  
    }
}

// === Tests ================================================================================== //

#[cfg(test)]
mod tests {
    use crate::logic::partial::parse::Parser;
    use super::*;

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
        O ::= 'o'
        A ::= 'a'
        B ::= 'b' A 'r'
        start ::= U | (B 'c')* | 't'
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let input = "b a r c b a r c";
        let past = p.partial(input).unwrap();
        
        println!("Partial AST root: {}", past.root().name);
        println!("Number of alternatives: {}", past.root().alts.len());
        for (i, alt) in past.root().alts.iter().enumerate() {
            println!("  Alt {}: complete={}, cursor={}, rhs_len={}, is_progressing={}", 
                i, alt.is_complete(), alt.cursor(), alt.production.rhs.len(), alt.is_progressing());
            if let Some(first_sym) = alt.production.rhs.first() {
                println!("    First symbol: {:?}", first_sym);
            }
            println!("    Slots: {:?}", alt.slots.keys().collect::<Vec<_>>());
            // Check what's in slot 0
            if let Some(slot) = alt.slots.get(&0) {
                match slot {
                    Slot::Filled{nodes,..} => println!("      Slot[0]: Filled with {} nodes", nodes.len()),
                    Slot::Partial { node, partial_symbol,.. } => {
                        println!("      Slot[0]: Partial, partial_symbol={:?}", partial_symbol.symbol_index());
                        if let Some(pnode) = node {
                            match pnode {
                                ParsedNode::Terminal(t) => println!("        Partial node: Terminal({})", t.value),
                                ParsedNode::NonTerminal(nt) => println!("        Partial node: NonTerminal({}, {} alts)", nt.name, nt.alts.len()),
                            }
                        } else {
                            println!("        Partial node: None");
                        }
                    }
                    Slot::Group { iterations, partial_iteration, group_size,.. } => {
                        println!("      Slot[0]: Group with {} iterations, partial_iteration={:?}, group_size={}",
                         iterations.len(), 
                         partial_iteration.as_ref().map(|s| s.iter().count()), 
                         group_size
                        );
                    }

                }
            }
        }
        
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
        A(ruleA) ::= 'a' 'x'?
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
        BinaryOp ::= AtomicExpr[left] ('+' | '-' | '*' | '/')[op] AtomicExpr[right]
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
        start ::= 'a'+
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
        start ::= ('a')? 'b'
        "#;
        let completions = complete(spec, "");
        println!("{:#?}", completions);
        assert!(completions.matches("a"), "nullable group allows 'a'");
        assert!(completions.matches("b"), "nullable group allows lookahead 'b'");
    }

    #[test]
    fn completion_group_repetition_tail() {
        let spec = r#"
        start ::= ('a' 'b')* 'c'
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
        assert_eq!(count_x, 1, "expected deduplication of identical 'x' suggestions");
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
        assert_eq!(completions.tokens.len(), 1, "should have exactly one completion");
    }

    #[test]
    fn completion_nested_nullable_groups() {
        let spec = r#"
        start ::= ('a')? ('b')? 'c'
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
        start ::= ('a' 'b')? 'c' 'd'
        "#;
        let completions = complete(spec, "");
        // Group is optional, so should suggest both 'a' (from group) and 'c' (skipping group)
        assert!(completions.matches("a"), "should suggest 'a' from optional group");
        assert!(completions.matches("c"), "should suggest 'c' (skipping optional group)");
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
        assert_eq!(completions.tokens.len(), 3, "should have exactly 3 completions");
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
        assert!(completions.matches("d"), "should find 'd' through nested nonterminals");
        assert_eq!(completions.tokens.len(), 1, "should have exactly one completion");
    }

    #[test]
    fn completion_partial_literal_midway() {
        let spec = r#"
        start ::= 'hello' 'world'
        "#;
        let completions = complete(spec, "hello");
        // After matching first literal, should suggest second
        assert!(completions.matches("world"), "should suggest 'world' after 'hello'");
    }

    #[test]
    fn completion_star_repetition_can_skip() {
        let spec = r#"
        A ::= 'a'
        start ::= A* 'b'
        "#;
        let completions = complete(spec, "");
        // * allows zero matches, so both 'a' and 'b' are valid
        assert!(completions.matches("a"), "should suggest 'a' from repetition");
        assert!(completions.matches("b"), "should suggest 'b' since * is nullable");
    }

    #[test]
    fn completion_plus_repetition_after_one_match() {
        let spec = r#"
        A ::= 'a'
        start ::= A+ 'b'
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
        start ::= Foo? 'bar'
        "#;
        let completions = complete(spec, "");
        // Optional nonterminal's FIRST and following symbol
        assert!(completions.matches("foo"), "should suggest 'foo' from optional Foo");
        assert!(completions.matches("bar"), "should suggest 'bar' since Foo is optional");
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
        assert!(completions.matches("[a-z]+"), "should suggest identifier regex");
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
        assert!(completions.matches("[0-9]+"), "should suggest regex after 'let'");
    }

    #[test]
    fn completion_multiple_completed_repetitions() {
        let spec = r#"
        start ::= ('a')* ('b')* 'c'
        "#;
        let completions = complete(spec, "aabb");
        // After matching 'aa' and 'bb', can continue either repetition or move to 'c'
        assert!(completions.matches("a"), "can continue first repetition");
        assert!(completions.matches("b"), "can continue second repetition");
        assert!(completions.matches("c"), "can move to 'c'");
    }

    #[test]
    fn completion_group_with_multiple_symbols() {
        let spec = r#"
        start ::= ('a' 'b' 'c')+ 'd'
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
        assert_eq!(completions.tokens.len(), 1, "should have exactly one completion");
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
        
;
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
        assert!(completions1.matches("[a-z]+"), "should suggest var at start");
        
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
        assert!(completions.matches("[0-9]+"), "should suggest number after +");
    }

    #[test]
    fn test_paren_expr_is_complete() {
        let spec = r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        
        Literal(lit) ::= Number[n]
        Variable(var) ::= Identifier[x]
        
        AtomicExpr ::= Literal | Variable | '(' Expression ')'
        BinaryOp(binop) ::= AtomicExpr[left] ('+' | '-' | '*' | '/')[op] AtomicExpr[right]
        
        Expression ::= AtomicExpr | BinaryOp
        "#;
        
        let grammar = crate::logic::grammar::Grammar::load(spec).unwrap();
        
        crate::set_debug_level(crate::DebugLevel::Trace);
        crate::set_debug_input(Some("(42)".to_string()));
        
        let mut parser1 = crate::logic::Parser::new(grammar.clone());
        let partial1 = parser1.partial("42").unwrap();
        println!("=== Parsing '42' ===");
        println!("Complete: {}", partial1.complete());
        println!("Alternatives: {}", partial1.root().alts.len());
        
        // Second test: parse "(42" - this is where the issue is
        let mut parser2 = crate::logic::Parser::new(grammar.clone());
        let partial2 = parser2.partial("(42").unwrap();
        println!("\n=== Parsing '(42' ===");
        println!("Complete: {}", partial2.complete());
        println!("Root nonterminal: {}", partial2.root().name);
        println!("Alternatives: {}", partial2.root().alts.len());
        
        for (i, alt) in partial2.root().alts.iter().enumerate() {
            println!("Alt {}: complete={}, progressing={}", 
                i, alt.is_complete(), alt.is_progressing());
            if let Some(rule) = &alt.production.rule {
                println!("  Rule: {}", rule);
            }
        }
        
        // Completions
        let completions = partial2.completions(&grammar);
        println!("Completions for '(42': {:?}", completions);
    }
}
