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

/// Cache for a single (depth, nt, start, searched_max_len) key.
///
/// `searched_max_len` is the number of segments that were available when we
/// ran the exhaustive parse.  We can only serve a cached answer for a query
/// with the *same* `max_len`, because with fewer/more tokens available the
/// parser might find different (shorter/longer) trees.
#[derive(Default, Debug, Clone)]
struct SpanCacheDepth {
    // nt -> start -> searched_max_len -> trees (by consumed_len)
    by_nt: HashMap<String, HashMap<usize, HashMap<usize, Vec<NonTerminal>>>>,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct SpanCache {
    // max_recursion(depth) -> cache at that depth
    by_depth: HashMap<usize, SpanCacheDepth>,
}

impl SpanCache {
    pub fn clear(&mut self) {
        self.by_depth.clear();
    }

    /// Try to serve results from cache.
    ///
    /// Returns `Some(trees)` only if we have a record for **exactly** this
    /// `(depth, nt, start, max_len)` triple — meaning we previously ran an
    /// exhaustive parse with exactly `max_len` segments available.
    ///
    /// Returns `None` if no such record exists, signalling that a fresh parse
    /// is needed.
    pub fn collect(
        &self,
        depth: usize,
        nt: &str,
        start: usize,
        max_len: usize,
    ) -> Option<Vec<NonTerminal>> {
        let d = self.by_depth.get(&depth)?;
        let starts = d.by_nt.get(nt)?;
        let by_searched = starts.get(&start)?;
        // Only serve if we ran with *exactly* this max_len
        by_searched.get(&max_len).cloned()
    }

    /// Store parse results.
    ///
    /// `searched_max_len` is `segments.len()` — the number of tokens that
    /// were available when this parse ran.  Trees are stored as a flat `Vec`
    /// (all consumed lengths mixed together) because callers filter by
    /// consumed_segments themselves.
    pub fn store_span(
        &mut self,
        depth: usize,
        nt: &str,
        start: usize,
        searched_max_len: usize,
        trees: Vec<NonTerminal>,
    ) {
        let d = self.by_depth.entry(depth).or_default();
        let starts = d.by_nt.entry(nt.to_string()).or_default();
        let by_searched = starts.entry(start).or_default();

        let existing = by_searched.entry(searched_max_len).or_default();
        for tree in trees {
            if !existing.contains(&tree) {
                existing.push(tree);
            }
        }
    }
}
