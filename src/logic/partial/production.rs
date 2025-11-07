use crate::logic::ast::SegmentRange;
use crate::logic::grammar::{Production, RepetitionKind, Symbol};
use crate::logic::tokenizer::Segment;
use crate::regex::Regex as DerivativeRegex;
use regex::Regex as ExternalRegex;

#[derive(Clone, Debug)]
pub enum PartialSymbol {
    Terminal {
        symbol_index: usize,
        span: SegmentRange,
        re: DerivativeRegex,
        derivative: DerivativeRegex,
    },
    NonTerminal {
        symbol_index: usize,
        nt: String,
    },
}

impl PartialSymbol {
    /// Get the text for this partial symbol from the segments
    pub fn current_text_from_segments(&self, segments: &[Segment]) -> String {
        match self {
            PartialSymbol::Terminal { span, .. } => {
                let mut result = String::new();
                for idx in span.start..=span.end {
                    if let Some(seg) = segments.get(idx) {
                        result.push_str(&seg.text());
                    }
                }
                result
            }
            _ => String::new(),
        }
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self, PartialSymbol::Terminal { .. })
    }

    pub fn symbol_index(&self) -> usize {
        match self {
            PartialSymbol::Terminal { symbol_index, .. } => *symbol_index,
            PartialSymbol::NonTerminal { symbol_index, .. } => *symbol_index,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PartialProduction {
    pub(crate) production: Production,
    pub(crate) fully_parsed_symbols: usize,
    pub(crate) partially_parsed_symbols: usize,
    pub(crate) partial_symbol: Option<PartialSymbol>,
}

impl PartialProduction {
    pub fn from_progress(
        production: Production,
        fully_parsed: usize,
        partially_parsed: usize,
        partial_symbol: Option<PartialSymbol>,
    ) -> Self {
        Self {
            production,
            fully_parsed_symbols: fully_parsed,
            partially_parsed_symbols: partially_parsed,
            partial_symbol,
        }
    }

    fn is_finished(&self) -> bool {
        self.fully_parsed_symbols == self.production.rhs.len() && self.partially_parsed_symbols == 0
    }
    pub fn complete(&self, children_count: usize) -> bool {
        self.is_finished() && children_count > 0
    }

    pub fn symbols(&self) -> Vec<Symbol> {
        self.production
            .rhs
            .get(self.fully_parsed_symbols..)
            .unwrap_or(&[])
            .to_vec()
    }

    pub fn set_cursor(&mut self, cursor: usize) {
        self.fully_parsed_symbols = cursor;
        self.partially_parsed_symbols = 0;
    }

    pub fn set_progress(&mut self, fully_parsed: usize, partially_parsed: usize) {
        self.fully_parsed_symbols = fully_parsed;
        self.partially_parsed_symbols = if partially_parsed > 0 { 1 } else { 0 };
    }

    pub fn fully_parsed_symbols_count(&self) -> usize {
        self.fully_parsed_symbols
    }
    pub fn has_partial_in_progress(&self) -> bool {
        self.partially_parsed_symbols > 0
    }
    pub fn rhs_len(&self) -> usize {
        self.production.rhs.len()
    }
    pub fn cursor_value(&self) -> usize {
        self.fully_parsed_symbols + self.partially_parsed_symbols
    }
    pub fn is_complete_state(&self, children_count: usize) -> bool {
        self.complete(children_count)
    }
    pub fn rhs_symbols(&self) -> &Vec<Symbol> {
        &self.production.rhs
    }
    pub fn rule_name(&self) -> Option<&String> {
        self.production.rule.as_ref()
    }
    pub fn partial_symbol(&self) -> &Option<PartialSymbol> {
        &self.partial_symbol
    }

    /// Borrow the in-progress partial symbol if present.
    pub fn partial_symbol_ref(&self) -> Option<&PartialSymbol> {
        self.partial_symbol.as_ref()
    }

    pub fn next_symbol_index(&self) -> Option<usize> {
        let idx = self.fully_parsed_symbols;
        if idx < self.production.rhs.len() {
            Some(idx)
        } else {
            None
        }
    }
    pub fn symbol_at(&self, index: usize) -> Option<&Symbol> {
        self.production.rhs.get(index)
    }

    pub fn base_symbol<'a>(&self, mut sym: &'a Symbol) -> &'a Symbol {
        loop {
            match sym {
                Symbol::Single { value, .. } => {
                    sym = value.as_ref();
                }
                _ => return sym,
            }
        }
    }
    pub fn base_symbol_at(&self, index: usize) -> Option<&Symbol> {
        self.symbol_at(index).map(|s| self.base_symbol(s))
    }

    pub fn in_progress_info(&self) -> Option<(usize, &PartialSymbol, &Symbol)> {
        match &self.partial_symbol {
            Some(ps) => self
                .base_symbol_at(ps.symbol_index())
                .map(|sym| (ps.symbol_index(), ps, sym)),
            None => None,
        }
    }

    /// Navigate into nested structures with a group path
    pub fn symbol_at_path<'a>(&'a self, index: usize, path: &'a [usize]) -> Option<&'a Symbol> {
        let mut sym = self.symbol_at(index)?;
        for step in path.iter().cloned() {
            match sym {
                Symbol::Single { value, .. } => {
                    sym = value.as_ref();
                }
                Symbol::Group { symbols, .. } => {
                    sym = symbols.get(step)?;
                }
                _ => return Some(sym),
            }
        }
        Some(sym)
    }
}
