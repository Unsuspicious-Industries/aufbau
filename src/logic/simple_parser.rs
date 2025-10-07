//! A drastically simplified recursive-descent parser.
//!
//! Repetition and partial-parse support were removed.  The implementation
//! is intentionally minimal and currently accepts only productions without
//! inline groups.  It is suitable for bootstrapping the new, simpler
//! pipeline; missing capabilities can be added incrementally.

use crate::logic::ast::{ASTNode, NonTerminal, Terminal};
use crate::logic::grammar::{Grammar, Symbol};
use crate::logic::tokenizer::Tokenizer;
use crate::{debug_error, debug_trace};

#[derive(Debug)]
pub struct Parser {
    grammar: Grammar,
    tokenizer: Tokenizer,
}

impl Parser {
    /// Create a new parser for a grammar.
    pub fn new(grammar: Grammar) -> Self {
        let specials = grammar.special_tokens.clone();
        Self {
            grammar,
            tokenizer: Tokenizer::new(specials, vec![' ', '\n', '\t']),
        }
    }

    /// Parse an entire input string starting from the grammar start symbol.
    pub fn parse(&mut self, input: &str) -> Result<ASTNode, String> {
        let start = self
            .grammar
            .start_nonterminal()
            .ok_or("Grammar has no start symbol")?
            .clone();
        let segments = self.tokenizer.tokenize_with_spans(input).map_err(|e| e.to_string())?;
        let tokens: Vec<&str> = segments.iter().map(|(tok, _, _)| *tok).collect();
        let (ast, pos) = self.parse_nt(&start, &tokens, 0)?;
        if pos != tokens.len() {
            return Err("input not fully consumed".into());
        }
        Ok(ast)
    }

    /// Parse a non-terminal at position `pos`.
    fn parse_nt<'a>(
        &self,
        nt: &str,
        tokens: &[&'a str],
        pos: usize,
    ) -> Result<(ASTNode, usize), String> {
        let prods = self
            .grammar
            .productions
            .get(nt)
            .ok_or_else(|| format!("unknown non-terminal '{}'", nt))?;
        for prod in prods {
            if let Ok((children, next)) = self.parse_seq(&prod.rhs, tokens, pos) {
                let node = ASTNode::Nonterminal(NonTerminal {
                    value: nt.to_string(),
                    span: None,
                    children,
                    binding: None,
                    bound_typing_rule: None,
                });
                return Ok((node, next));
            }
        }
        Err(format!("no production of '{}' matched at {}", nt, pos))
    }

    /// Parse a sequence of symbols.
    fn parse_seq<'a>(
        &self,
        symbols: &[Symbol],
        tokens: &[&'a str],
        mut pos: usize,
    ) -> Result<(Vec<ASTNode>, usize), String> {
        let mut children = Vec::new();
        for sym in symbols {
            let (node, new_pos) = self.parse_sym(sym, tokens, pos)?;
            children.push(node);
            pos = new_pos;
        }
        Ok((children, pos))
    }

    /// Parse a single symbol.
    fn parse_sym<'a>(
        &self,
        sym: &Symbol,
        tokens: &[&'a str],
        pos: usize,
    ) -> Result<(ASTNode, usize), String> {
        if pos >= tokens.len() {
            return Err("unexpected end of input".into());
        }
        match sym {
            Symbol::Litteral(lit) => {
                if tokens[pos] == lit {
                    Ok((
                        ASTNode::Terminal(Terminal {
                            value: lit.clone(),
                            span: None,
                            binding: None,
                        }),
                        pos + 1,
                    ))
                } else {
                    Err(format!("expected '{}', found '{}'", lit, tokens[pos]))
                }
            }
            Symbol::Regex(re) => {
                if re.is_match(tokens[pos]) {
                    Ok((
                        ASTNode::Terminal(Terminal {
                            value: tokens[pos].to_string(),
                            span: None,
                            binding: None,
                        }),
                        pos + 1,
                    ))
                } else {
                    Err(format!("token '{}' did not match regex", tokens[pos]))
                }
            }
            Symbol::Expression(nt) => self.parse_nt(nt, tokens, pos),
            Symbol::Single { value, .. } => self.parse_sym(value, tokens, pos),
            Symbol::Group { symbols } => {
                let (nodes, next) = self.parse_seq(symbols, tokens, pos)?;
                Ok((ASTNode::Nonterminal(NonTerminal {
                    value: "(group)".into(),
                    span: None,
                    children: nodes,
                    binding: None,
                    bound_typing_rule: None,
                }), next))
            }
        }
    }
}
