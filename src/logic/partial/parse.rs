use std::collections::HashSet;

use crate::logic::ast::{ASTNode, SegmentRange};
use crate::logic::grammar::{Grammar, Production, Symbol};
use crate::logic::partial::{Node, NonTerminal, PartialAST, Terminal};
use crate::logic::tokenizer::{Segment, Tokenizer};
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use crate::{debug_debug, debug_trace};

impl Segment {
    /// Get the segment range (just its own index)
    pub fn seg_range(&self) -> SegmentRange {
        SegmentRange::single(self.index)
    }
}

pub struct Parser {
    pub (crate) grammar: Grammar,
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let specials = grammar.special_tokens.clone();
        let validation_regex = grammar.accepted_tokens_regex.clone();
        Self {
            grammar,
            tokenizer: Tokenizer::new(specials, vec![' ', '\n', '\t'], validation_regex),
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<ASTNode, String> {
        let past = self
            .partial(input)
            .map_err(|e| format!("Parse error: {}", e))?;

        // Re-tokenize to determine how many segments the full input contributes.
        let segments = self.tokenize(input)?;
        let total_segments = segments.len();

        // Find a complete tree that consumed all segments
        let complete_root = past
            .roots
            .iter()
            .find(|r| r.is_complete() && r.consumed_segments == total_segments);

        if let Some(_) = complete_root {
            past.into_complete()
                .map_err(|e| format!("Incomplete parse: {}", e))
        } else {
            Err(format!(
                "Parse error: no complete parse found consuming all {} tokens",
                total_segments
            ))
        }
    }

    /// Main entry point: parse input and return new PartialAST
    pub fn partial(&mut self, input: &str) -> Result<PartialAST, String> {
        debug_trace!("parser2      ", "Starting parse of input: '{}'", input);

        // Tokenize
        let segments = self.tokenize(input)?;
        debug_debug!("parser2      ", "Tokenized into {:?}", segments);

        // Get start nonterminal
        let start_nt = self
            .grammar
            .start_nonterminal()
            .ok_or_else(|| "No start nonterminal in grammar".to_string())?;

        debug_debug!("parser2      ", "Start nonterminal: {}", start_nt);

        // Parse from start
        let mut visited = HashSet::new();
        let roots = self.parse_nonterminal(&segments, start_nt, None, 0, &mut visited)?;

        let total_segments = segments.len();

        // Filter roots that consumed all input
        let valid_roots: Vec<NonTerminal> = roots
            .into_iter()
            .filter(|r| r.consumed_segments == total_segments)
            .collect();

        if valid_roots.is_empty() {
            debug_debug!(
                "parser2      ",
                "No alternatives consuming {} segments for start symbol '{}'",
                total_segments,
                start_nt
            );
            return Err("No valid parse alternatives found".to_string());
        }

        let ast = PartialAST::new(valid_roots, input.to_string());

        Ok(ast)
    }

    /// Tokenize input into segments
    fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        self.tokenizer
            .tokenize(input)
            .map_err(|e| format!("Tokenization failed: {:?}", e))
    }

    /// Parse a nonterminal: try all productions, return all valid trees
    fn parse_nonterminal(
        &self,
        segments: &[Segment],
        nt_name: &str,
        binding: Option<String>,
        level: usize,
        visited: &mut HashSet<(String, usize)>,
    ) -> Result<Vec<NonTerminal>, String> {
        let indent = "  ".repeat(level);
        debug_trace!(
            "parser2      ",
            "{}[L{}] Parsing nonterminal '{}'",
            indent,
            level,
            nt_name
        );

        // Check for recursion on same input position
        let key = (nt_name.to_string(), segments.len());
        if visited.contains(&key) {
            debug_trace!(
                "parser2      ",
                "{}[L{}] Recursion detected for '{}' at len {}",
                indent,
                level,
                nt_name,
                segments.len()
            );
            return Ok(Vec::new());
        }
        visited.insert(key.clone());

        let productions = self
            .grammar
            .productions
            .get(nt_name)
            .ok_or_else(|| format!("No productions for nonterminal '{}'", nt_name))?;

        let mut results = Vec::new();

        for (alt_idx, prod) in productions.iter().enumerate() {
            debug_trace!(
                "parser2      ",
                "{}[L{}] Trying production: {:?}",
                indent,
                level,
                prod.rhs
            );

            match self.parse_production(segments, prod, level, visited) {
                Ok(prod_results) => {
                    for children in prod_results {
                        let consumed = self.count_consumed_segments(&children);
                        let nt = NonTerminal::new(
                            nt_name.to_string(),
                            prod.clone(),
                            alt_idx,
                            children,
                            binding.clone(),
                            consumed,
                        );
                        debug_trace!(
                            "parser2      ",
                            "{}[L{}] Production succeeded: complete={}",
                            indent,
                            level,
                            nt.is_complete()
                        );
                        results.push(nt);
                    }
                }
                Err(e) => {
                    debug_trace!(
                        "parser2      ",
                        "{}[L{}] Production error: {}",
                        indent,
                        level,
                        e
                    );
                }
            }
        }

        visited.remove(&key);

        debug_trace!(
            "parser2      ",
            "{}[L{}] Finished parsing nonterminal '{}': {} trees",
            indent,
            level,
            nt_name,
            results.len()
        );

        Ok(results)
    }

    fn parse_production(
        &self,
        segments: &[Segment],
        prod: &Production,
        level: usize,
        visited: &mut HashSet<(String, usize)>,
    ) -> Result<Vec<Vec<Node>>, String> {
        let indent = "  ".repeat(level);
        debug_trace!(
            "parser2.prod ",
            "{}[L{}] Parsing production: {:?}",
            indent,
            level,
            prod
        );

        if prod.rhs.is_empty() {
            debug_trace!(
                "parser2.prod ",
                "{}[L{}] Epsilon production matched",
                indent,
                level
            );
            return Ok(vec![vec![]]);
        }

        self.parse_symbols(segments, &prod.rhs, level, visited)
    }

    fn parse_symbols(
        &self,
        segments: &[Segment],
        symbols: &[Symbol],
        level: usize,
        visited: &mut HashSet<(String, usize)>,
    ) -> Result<Vec<Vec<Node>>, String> {
        if symbols.is_empty() {
            return Ok(vec![vec![]]);
        }

        let first_sym = &symbols[0];
        let rest_syms = &symbols[1..];

        let first_parses = self.parse_symbol(segments, first_sym, level, visited)?;

        if first_parses.is_empty() {
            return Ok(Vec::new());
        }

        let mut results = Vec::new();

        for node in first_parses {
            // If this node is partial, we cannot continue parsing subsequent symbols
            // in this production because the prefix is not complete.
            if !node.is_complete() {
                results.push(vec![node]);
                continue;
            }

            let consumed = self.node_consumed(&node);
            let remaining_segments = if consumed >= segments.len() {
                &[]
            } else {
                &segments[consumed..]
            };

            let rest_parses = self.parse_symbols(remaining_segments, rest_syms, level, visited)?;

            for mut rest_nodes in rest_parses {
                let mut full_parse = vec![node.clone()];
                full_parse.append(&mut rest_nodes);
                results.push(full_parse);
            }
        }

        Ok(results)
    }

    fn node_consumed(&self, node: &Node) -> usize {
        match node {
            Node::Terminal(Terminal::Complete { .. }) => 1,
            Node::Terminal(Terminal::Partial { value, .. }) => {
                if !value.is_empty() {
                    1
                } else {
                    0
                }
            }
            Node::NonTerminal(nt) => nt.consumed_segments,
        }
    }

    fn count_consumed_segments(&self, nodes: &[Node]) -> usize {
        nodes.iter().map(|n| self.node_consumed(n)).sum()
    }

    /// Parse a symbol (expression or regex)
    fn parse_symbol(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        level: usize,
        visited: &mut HashSet<(String, usize)>,
    ) -> Result<Vec<Node>, String> {
        let res = match symbol {
            Symbol::Regex { regex, binding } => {
                self.parse_regex(segments, regex, binding.clone(), level)
            }
            Symbol::Expression { name, binding } => {
                let nts =
                    self.parse_nonterminal(segments, name, binding.clone(), level + 1, visited)?;
                Ok(nts.into_iter().map(Node::NonTerminal).collect())
            }
        };
        res
    }

    /// Parse regex terminal
    fn parse_regex(
        &self,
        segments: &[Segment],
        re: &DerivativeRegex,
        binding: Option<String>,
        level: usize,
    ) -> Result<Vec<Node>, String> {
        if segments.is_empty() {
            // At end of input - partial match with remainder
            debug_trace!(
                "parser2.regex",
                "{}[L{}] At end of input, returning partial terminal",
                "  ".repeat(level),
                level
            );
            let node = Node::Terminal(Terminal::Partial {
                value: String::new(),
                binding: binding.clone(),
                remainder: Some(re.clone()),
            });
            return Ok(vec![node]);
        }

        let seg = &segments[0];
        let indent = "  ".repeat(level);
        debug_trace!(
            "parser2.regex",
            "{}[L{}] Trying regex '{}' against segment '{}'",
            indent,
            level,
            re.to_pattern(),
            seg.text()
        );

        let node = match re.prefix_match(&seg.text()) {
            PrefixStatus::Complete => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex FULL match for segment '{}'",
                    indent,
                    level,
                    seg.text()
                );
                Some(Node::Terminal(Terminal::Complete {
                    value: seg.text().to_string(),
                    binding: binding.clone(),
                    extension: None,
                }))
            }
            PrefixStatus::Prefix(derivative) => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex PARTIAL match for segment '{}'",
                    indent,
                    level,
                    seg.text()
                );
                Some(Node::Terminal(Terminal::Partial {
                    value: seg.text().to_string(),
                    binding: binding.clone(),
                    remainder: Some(derivative.clone()),
                }))
            }
            PrefixStatus::Extensible(derivative) => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex EXTENSIBLE match for segment '{}'",
                    indent,
                    level,
                    seg.text()
                );
                Some(Node::Terminal(Terminal::Complete {
                    value: seg.text().to_string(),
                    binding: binding.clone(),
                    extension: Some(derivative.clone()),
                }))
            }
            PrefixStatus::NoMatch => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex NO match for segment '{}'",
                    indent,
                    level,
                    seg.text()
                );
                None
            }
        };

        Ok(node.into_iter().collect())
    }
}

#[cfg(test)]
mod tests {
    use crate::set_debug_level;

    use super::*;

    #[test]
    fn test_simple_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let ast = p.partial("hello").unwrap();
        assert!(ast.complete());
        println!("AST: {:?}", ast);
    }

    #[test]
    fn test_partial_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        set_debug_level(crate::DebugLevel::Debug);
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let ast = p.partial("hel").unwrap();
        assert!(!ast.complete());
    }

    #[test]
    fn test_alternatives() {
        let spec = r#"
        A ::= 'a'
        B ::= 'b'
        start ::= A | B
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let ast = p.partial("a").unwrap();
        std::println!("AST: {:?}", ast);
        assert!(ast.complete());
        assert_eq!(ast.roots.len(), 1);
        assert_eq!(ast.roots[0].name, "start");
        // Check children
        let child = &ast.roots[0].children[0];
        if let Node::NonTerminal(nt) = child {
            assert_eq!(nt.name, "A");
        } else {
            panic!("Expected NonTerminal A");
        }
    }

    #[test]
    fn test_partial_alternatives() {
        let spec = r#"
        A ::= 'a'
        B ::= 'a' 'b'
        start ::= A | B
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let ast = p.partial("a").unwrap();
        std::println!("AST: {:?}", ast);
        // A: complete (matched 'a')
        // B: partial (matched 'a', missing 'b')
        // Both should be present as roots
        assert_eq!(ast.roots.len(), 2);
    }

    #[test]
    fn test_partial_at_end() {
        let spec = r#"
        start ::= 'hello' 'world'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let ast = p.partial("hello wor").unwrap();
        assert!(!ast.complete());
    }

    #[test]
    fn test_mismatch_rejection() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let _ast = p.partial("goodbye").unwrap_err();
    }

    #[test]
    fn test_complex_grammar() {
        let spec = r#"
        Number ::= /[0-9]+/
        Op ::= '+' | '-'
        Expr ::= Number | Number Op Expr
        start ::= Expr
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let ast = p.partial("1 + 2 - 3").unwrap();
        assert!(ast.complete());
    }

    #[test]
    fn test_binding_preservation() {
        let spec = r#"
        Number ::= /[0-9]+/
        start ::= Number[x]
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);

        let ast = p.partial("42").unwrap();
        println!("AST: {:#?}", ast);
        assert!(ast.complete());

        let root = &ast.roots[0];
        let child = &root.children[0];
        if let Node::NonTerminal(nt) = child {
            assert_eq!(nt.binding, Some("x".to_string()));
        } else {
            panic!("Expected NonTerminal node");
        }
    }
}
