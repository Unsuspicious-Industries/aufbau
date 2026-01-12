//! String generation and caching.

use super::Regex;
use moka::sync::Cache;
use once_cell::sync::Lazy;
use std::collections::HashSet;
use std::sync::Arc;

static CACHE: Lazy<Cache<(Regex, usize), Arc<HashSet<String>>>> =
    Lazy::new(|| Cache::builder().max_capacity(10_000).build());

pub fn clear_cache() {
    CACHE.invalidate_all();
}
pub fn cache_stats() -> (u64, u64) {
    (CACHE.entry_count(), CACHE.weighted_size())
}

pub fn valids(r: &Regex, max: usize) -> HashSet<String> {
    let key = (r.clone(), max);
    if let Some(c) = CACHE.get(&key) {
        return (*c).clone();
    }
    let res: HashSet<_> = (0..=max).flat_map(|n| valids_n(r, n)).collect();
    CACHE.insert(key, Arc::new(res.clone()));
    res
}

fn valids_n(r: &Regex, n: usize) -> HashSet<String> {
    match r {
        Regex::Empty => HashSet::new(),
        Regex::Epsilon => {
            if n == 0 {
                ["".into()].into()
            } else {
                HashSet::new()
            }
        }
        Regex::Char(c) => {
            if n == 1 {
                [c.to_string()].into()
            } else {
                HashSet::new()
            }
        }
        Regex::Range(lo, hi) => {
            if n == 1 {
                ((*lo as u32)..=(*hi as u32))
                    .filter_map(char::from_u32)
                    .map(|c| c.to_string())
                    .collect()
            } else {
                HashSet::new()
            }
        }
        Regex::Union(a, b) => valids_n(a, n).into_iter().chain(valids_n(b, n)).collect(),
        Regex::Concat(a, b) => (0..=n)
            .flat_map(|i| {
                let (l, r) = (valids_n(a, i), valids_n(b, n - i));
                l.iter()
                    .flat_map(|x| r.iter().map(move |y| format!("{x}{y}")))
                    .collect::<Vec<_>>()
            })
            .collect(),
        Regex::Star(inner) => {
            if n == 0 {
                ["".into()].into()
            } else {
                (1..=n)
                    .flat_map(|i| {
                        let (first, rest) = (valids_n(inner, i), valids_n(r, n - i));
                        first
                            .iter()
                            .flat_map(|f| rest.iter().map(move |t| format!("{f}{t}")))
                            .collect::<Vec<_>>()
                    })
                    .collect()
            }
        }
    }
}
