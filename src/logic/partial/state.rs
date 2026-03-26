use crate::logic::grammar::Segment;
use crate::logic::partial::memo::{stable_memo, MemoTable, ParseMemoKey};
use crate::logic::partial::structure::SppfForest;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct PrefixState {
    input: String,
    segments: Vec<Segment>,
    forest: SppfForest,
    memo: MemoTable,
    frontier: Vec<ParseMemoKey>,
    hit_depth_limit: bool,
    max_recursion: usize,
}

impl PrefixState {
    pub(crate) fn new(
        input: String,
        segments: Vec<Segment>,
        forest: SppfForest,
        memo: MemoTable,
        frontier: Vec<ParseMemoKey>,
        hit_depth_limit: bool,
        max_recursion: usize,
    ) -> Self {
        Self {
            input,
            segments,
            forest,
            memo,
            frontier,
            hit_depth_limit,
            max_recursion,
        }
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    pub fn forest(&self) -> &SppfForest {
        &self.forest
    }

    pub fn into_forest(self) -> SppfForest {
        self.forest
    }

    pub(crate) fn stable_memo(&self) -> MemoTable {
        stable_memo(&self.memo)
    }

    pub(crate) fn into_stable_memo(self) -> MemoTable {
        stable_memo(&self.memo)
    }

    pub fn frontier_size(&self) -> usize {
        self.frontier.len()
    }

    pub fn hit_depth_limit(&self) -> bool {
        self.hit_depth_limit
    }

    pub fn max_recursion(&self) -> usize {
        self.max_recursion
    }
}

pub(crate) struct ParseState {
    pub memo: MemoTable,
    pub active: HashSet<ParseMemoKey>,
    pub frontier: HashSet<ParseMemoKey>,
    pub hit_depth_limit: bool,
}

impl ParseState {
    pub fn with_seed(memo: MemoTable) -> Self {
        Self {
            memo,
            active: HashSet::new(),
            frontier: HashSet::new(),
            hit_depth_limit: false,
        }
    }
}
