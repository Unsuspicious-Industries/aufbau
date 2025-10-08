use crate::logic::ast::SourceSpan;
use crate::logic::grammar::{Production, RepetitionKind, Symbol};
use regex::Regex;

#[derive(Clone, Debug)]
pub enum PartialSymbol {
    Litteral {
        symbol_index: usize,
        byte_cursor: usize,
        span: SourceSpan,
        expected: String,
    },
    Regex {
        symbol_index: usize,
        byte_cursor: usize,
        span: SourceSpan,
        re: Regex,
    },
    Expression {
        symbol_index: usize,
        nt: String,
    },
    Single {
        symbol_index: usize,
        repetition: Option<RepetitionKind>,
        binding: Option<String>,
    },
    Group {
        symbol_index: usize,
        /// If known, which inner symbol index within the group is targeted
        inner_index: Option<usize>,
        repetition: Option<RepetitionKind>,
        binding: Option<String>,
    },
    Other {
        symbol_index: usize,
    },
}

impl PartialSymbol {
    pub fn current_text<'a>(&self, input: &'a str) -> &'a str {
        match self {
            PartialSymbol::Litteral { span, .. } | PartialSymbol::Regex { span, .. } => {
                &input[span.start..span.end.min(input.len())]
            }
            _ => "",
        }
    }
    pub fn is_regex(&self) -> bool {
        matches!(self, PartialSymbol::Regex { .. })
    }
    pub fn is_litteral(&self) -> bool {
        matches!(self, PartialSymbol::Litteral { .. })
    }
    pub fn symbol_index(&self) -> usize {
        match self {
            PartialSymbol::Litteral { symbol_index, .. }
            | PartialSymbol::Regex { symbol_index, .. }
            | PartialSymbol::Expression { symbol_index, .. }
            | PartialSymbol::Single { symbol_index, .. }
            | PartialSymbol::Group { symbol_index, .. }
            | PartialSymbol::Other { symbol_index, .. } => *symbol_index,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PartialProduction {
    pub(crate) production: Production,
    pub(crate) fully_parsed_symbols: usize,
    pub(crate) partially_parsed_symbols: usize,
    pub(crate) partial_symbol: Option<PartialSymbol>,
    pub(crate) rhs: Vec<PartialSymbol>,
}

impl PartialProduction {
    pub fn new(production: Production) -> Self {
        let rhs = production
            .rhs
            .iter()
            .enumerate()
            .map(|(i, s)| match s {
                Symbol::Litteral(text) => PartialSymbol::Litteral {
                    symbol_index: i,
                    byte_cursor: 0,
                    span: SourceSpan { start: 0, end: 0 },
                    expected: text.clone(),
                },
                Symbol::Regex(re) => PartialSymbol::Regex {
                    symbol_index: i,
                    byte_cursor: 0,
                    span: SourceSpan { start: 0, end: 0 },
                    re: re.clone(),
                },
                Symbol::Expression(nt) => PartialSymbol::Expression {
                    symbol_index: i,
                    nt: nt.clone(),
                },
                Symbol::Single {
                    repetition,
                    binding,
                    ..
                } => PartialSymbol::Single {
                    symbol_index: i,
                    repetition: repetition.clone(),
                    binding: binding.clone(),
                },
                Symbol::Group { repetition, .. } => PartialSymbol::Group {
                    symbol_index: i,
                    inner_index: None,
                    repetition: repetition.clone(),
                    binding: None,
                },
            })
            .collect();
        Self {
            production,
            fully_parsed_symbols: 0,
            partially_parsed_symbols: 0,
            partial_symbol: None,
            rhs,
        }
    }

    pub fn from_progress(
        production: Production,
        fully_parsed: usize,
        partially_parsed: usize,
    ) -> Self {
        let mut pp = Self::new(production);
        pp.set_progress(fully_parsed, partially_parsed);
        pp
    }

    pub fn from_progress_with_partial_symbol(
        production: Production,
        fully_parsed: usize,
        partially_parsed: usize,
        partial_symbol: Option<PartialSymbol>,
    ) -> Self {
        let mut pp = Self::from_progress(production, fully_parsed, partially_parsed);
        if let Some(ps) = partial_symbol {
            let idx = ps.symbol_index();
            if idx < pp.rhs.len() {
                pp.rhs[idx] = ps.clone();
            }
            pp.partial_symbol = Some(ps);
        }
        pp
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
    pub fn rhs(&self) -> &Vec<PartialSymbol> {
        &self.rhs
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
