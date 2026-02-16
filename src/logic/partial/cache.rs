use std::collections::HashMap;

use super::NonTerminal;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SegmentKey {
    pub text: String,
    pub is_partial_special: bool,
}

pub fn is_prefix(prefix: &[SegmentKey], full: &[SegmentKey]) -> bool {
    if prefix.len() > full.len() {
        return false;
    }
    prefix.iter().zip(full.iter()).all(|(a, b)| a == b)
}

#[derive(Default, Debug, Clone)]
struct SpanCacheDepth {
    // nt -> start -> len -> trees
    by_nt: HashMap<String, HashMap<usize, HashMap<usize, Vec<NonTerminal>>>>,
    // nt -> start -> max_len fully computed for parse_nonterminal(nt, start, max_len)
    computed_max_len: HashMap<String, HashMap<usize, usize>>,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct SpanCache {
    // max_recursion(depth) -> cache at that depth
    by_depth: HashMap<usize, SpanCacheDepth>,
    total_span_buckets: u64,
    total_trees: u64,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct SpanCacheCounts {
    pub nt_count: u64,
    pub span_buckets: u64,
    pub stored_trees: u64,
}

impl SpanCache {
    pub fn clear(&mut self) {
        self.by_depth.clear();
        self.total_span_buckets = 0;
        self.total_trees = 0;
    }

    pub fn counts(&self) -> SpanCacheCounts {
        let mut nt_count: u64 = 0;

        for depth in self.by_depth.values() {
            nt_count += depth.by_nt.len() as u64;
        }

        SpanCacheCounts {
            nt_count,
            span_buckets: self.total_span_buckets,
            stored_trees: self.total_trees,
        }
    }

    pub fn total_span_buckets(&self) -> u64 {
        self.total_span_buckets
    }

    pub fn mark_computed(&mut self, depth: usize, nt: &str, start: usize, max_len: usize) {
        let d = self.by_depth.entry(depth).or_default();
        let start_map = d.computed_max_len.entry(nt.to_string()).or_default();
        let prev = start_map.get(&start).copied().unwrap_or(0);
        if max_len > prev {
            start_map.insert(start, max_len);
        }
    }

    pub fn computed_len(&self, depth: usize, nt: &str, start: usize) -> usize {
        self.by_depth
            .get(&depth)
            .and_then(|d| d.computed_max_len.get(nt))
            .and_then(|m| m.get(&start))
            .copied()
            .unwrap_or(0)
    }

    pub fn can_answer(&self, depth: usize, nt: &str, start: usize, max_len: usize) -> bool {
        self.by_depth
            .get(&depth)
            .and_then(|d| d.computed_max_len.get(nt))
            .and_then(|m| m.get(&start))
            .copied()
            .map_or(false, |have| have >= max_len)
    }

    pub fn collect(
        &self,
        depth: usize,
        nt: &str,
        start: usize,
        max_len: usize,
    ) -> (Vec<NonTerminal>, u64) {
        let d = match self.by_depth.get(&depth) {
            Some(d) => d,
            None => return (Vec::new(), 0),
        };
        let starts = match d.by_nt.get(nt) {
            Some(s) => s,
            None => return (Vec::new(), 0),
        };
        let lens = match starts.get(&start) {
            Some(l) => l,
            None => return (Vec::new(), 0),
        };

        let mut scanned: u64 = 0;
        let mut out: Vec<NonTerminal> = Vec::new();
        for (len, trees) in lens {
            scanned += 1;
            if *len <= max_len {
                out.extend(trees.iter().cloned());
            }
        }
        (out, scanned)
    }

    pub fn store_span(
        &mut self,
        depth: usize,
        nt: &str,
        start: usize,
        len: usize,
        trees: Vec<NonTerminal>,
    ) -> bool {
        let d = self.by_depth.entry(depth).or_default();
        let starts = d.by_nt.entry(nt.to_string()).or_default();
        let lens = starts.entry(start).or_default();
        let existed = lens.contains_key(&len);
        let prev_len = lens.get(&len).map(|v| v.len()).unwrap_or(0) as u64;
        lens.insert(len, trees);
        let new_len = lens.get(&len).map(|v| v.len()).unwrap_or(0) as u64;

        if !existed {
            self.total_span_buckets += 1;
        }
        self.total_trees = self.total_trees.saturating_sub(prev_len);
        self.total_trees = self.total_trees.saturating_add(new_len);
        existed
    }
}
