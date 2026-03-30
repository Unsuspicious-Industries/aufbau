use crate::logic::grammar::Segment;
use crate::logic::partial::memo::{MemoEntry, MemoTable, ParseMemoKey, ParsedNt};
use crate::logic::partial::structure::SppfForest;
use std::collections::HashSet;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub(crate) struct SeedMemo {
    pub memo: Arc<MemoTable>,
    pub frontier: Vec<ParseMemoKey>,
    pub total_segments: usize,
}

impl SeedMemo {
    pub fn empty() -> Self {
        Self {
            memo: Arc::new(MemoTable::new()),
            frontier: Vec::new(),
            total_segments: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrefixState {
    input: String,
    segments: Vec<Segment>,
    forest: SppfForest,
    memo: Arc<MemoTable>,
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
            memo: Arc::new(memo),
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

    pub(crate) fn seed_memo(&self) -> SeedMemo {
        SeedMemo {
            memo: Arc::clone(&self.memo),
            frontier: self.frontier.clone(),
            total_segments: self.segments.len(),
        }
    }

    pub(crate) fn into_seed_memo(self) -> SeedMemo {
        SeedMemo {
            memo: self.memo,
            frontier: self.frontier,
            total_segments: self.segments.len(),
        }
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
    pub seed_memo: Arc<MemoTable>,
    pub seed_frontier: HashSet<ParseMemoKey>,
    pub seed_total_segments: usize,
    pub active: HashSet<ParseMemoKey>,
    pub frontier: HashSet<ParseMemoKey>,
    pub hit_depth_limit: bool,
}

impl ParseState {
    pub fn with_seed(seed: SeedMemo) -> Self {
        Self {
            memo: MemoTable::new(),
            seed_memo: seed.memo,
            seed_frontier: seed.frontier.into_iter().collect(),
            seed_total_segments: seed.total_segments,
            active: HashSet::new(),
            frontier: HashSet::new(),
            hit_depth_limit: false,
        }
    }

    pub fn memoized(&self, key: &ParseMemoKey) -> Option<Vec<ParsedNt>> {
        self.memo.get(key).map(|entry| entry.all())
    }

    pub fn seed_entry(&self, key: &ParseMemoKey) -> Option<&MemoEntry> {
        self.seed_memo.get(key)
    }

    pub fn seed_outcomes(&self, key: &ParseMemoKey) -> Vec<ParsedNt> {
        self.seed_entry(key)
            .map(|entry| entry.all())
            .unwrap_or_default()
    }

    pub fn seed_entry_is_exact(&self, key: &ParseMemoKey) -> bool {
        let Some(entry) = self.seed_entry(key) else {
            return false;
        };

        self.seed_total_segments > 0
            && !self.seed_frontier.contains(key)
            && seed_exact(entry, key.abs_pos, self.seed_total_segments)
    }
}

fn seed_exact(entry: &MemoEntry, abs_pos: usize, total_segments: usize) -> bool {
    let remaining = total_segments.saturating_sub(abs_pos);
    !entry.has_partial()
        && entry
            .complete
            .iter()
            .all(|parsed| parsed.consumed < remaining)
}
