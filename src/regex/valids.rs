//! String generation and caching.

use super::Regex;
use moka::sync::Cache;
use once_cell::sync::Lazy;
use std::collections::HashSet;
use std::sync::Arc;

static CACHE: Lazy<Cache<(Regex, usize), Arc<HashSet<String>>>> =
    Lazy::new(|| Cache::builder().max_capacity(10_000).build());

pub fn clear_cache() { CACHE.invalidate_all(); }
pub fn cache_stats() -> (u64, u64) { (CACHE.entry_count(), CACHE.weighted_size()) }

pub fn valids(r: &Regex, max: usize) -> HashSet<String> {
    let key = (r.clone(), max);
    if let Some(c) = CACHE.get(&key) { return (*c).clone(); }
    let res: HashSet<_> = (0..=max).flat_map(|n| valids_n(r, n)).collect();
    CACHE.insert(key, Arc::new(res.clone()));
    res
}

fn valids_n(r: &Regex, n: usize) -> HashSet<String> {
    match r {
        Regex::Empty => HashSet::new(),
        Regex::Epsilon => if n == 0 { ["".into()].into() } else { HashSet::new() },
        Regex::Char(c) => if n == 1 { [c.to_string()].into() } else { HashSet::new() },
        Regex::Range(lo, hi) => if n == 1 {
            ((*lo as u32)..=(*hi as u32)).filter_map(char::from_u32).map(|c| c.to_string()).collect()
        } else { HashSet::new() },
        Regex::Union(a, b) => valids_n(a, n).into_iter().chain(valids_n(b, n)).collect(),
        Regex::Concat(a, b) => (0..=n).flat_map(|i| {
            let (l, r) = (valids_n(a, i), valids_n(b, n - i));
            l.iter().flat_map(|x| r.iter().map(move |y| format!("{x}{y}"))).collect::<Vec<_>>()
        }).collect(),
        Regex::Star(inner) => if n == 0 { ["".into()].into() } else {
            (1..=n).flat_map(|i| {
                let (first, rest) = (valids_n(inner, i), valids_n(r, n - i));
                first.iter().flat_map(|f| rest.iter().map(move |t| format!("{f}{t}"))).collect::<Vec<_>>()
            }).collect()
        },
    }
}

pub fn example(r: &Regex) -> Option<String> {
    match r {
        Regex::Empty => None,
        Regex::Epsilon | Regex::Star(_) => Some(String::new()),
        Regex::Char(c) => Some(c.to_string()),
        Regex::Range(lo, _) => Some(lo.to_string()),
        Regex::Concat(a, b) => Some(format!("{}{}", example(a)?, example(b)?)),
        Regex::Union(a, b) => example(a).or_else(|| example(b)),
    }
}

pub fn examples(r: &Regex, n: usize) -> Vec<String> {
    let mut out = Vec::with_capacity(n);
    collect(r, n, &mut out);
    out
}

fn collect(r: &Regex, n: usize, out: &mut Vec<String>) {
    if out.len() >= n { return; }
    match r {
        Regex::Empty => {},
        Regex::Epsilon => push_unique(out, String::new()),
        Regex::Char(c) => push_unique(out, c.to_string()),
        Regex::Range(lo, hi) => {
            for c in (*lo as u32)..=(*hi as u32) {
                if out.len() >= n { break; }
                if let Some(ch) = char::from_u32(c) { push_unique(out, ch.to_string()); }
            }
        },
        Regex::Union(a, b) => {
            let (mut va, mut vb) = (vec![], vec![]);
            collect(a, n, &mut va);
            collect(b, n, &mut vb);
            let mut i = 0;
            while out.len() < n && (i < va.len() || i < vb.len()) {
                if i < va.len() { push_unique(out, va[i].clone()); }
                if out.len() < n && i < vb.len() { push_unique(out, vb[i].clone()); }
                i += 1;
            }
        },
        Regex::Concat(a, b) => {
            let k = ((n as f64).sqrt().ceil() as usize).max(2);
            let (mut va, mut vb) = (vec![], vec![]);
            collect(a, k, &mut va);
            collect(b, k, &mut vb);
            'done: for x in &va {
                for y in &vb {
                    if out.len() >= n { break 'done; }
                    push_unique(out, format!("{x}{y}"));
                }
            }
        },
        Regex::Star(inner) => {
            push_unique(out, String::new());
            let mut sub = vec![];
            collect(inner, n, &mut sub);
            for rep in 1..=3 {
                if out.len() >= n { break; }
                for combo in combos(&sub, rep) {
                    if out.len() >= n { break; }
                    push_unique(out, combo.into_iter().cloned().collect());
                }
            }
        },
    }
}

fn push_unique(out: &mut Vec<String>, s: String) { if !out.contains(&s) { out.push(s); } }

fn combos(items: &[String], k: usize) -> Vec<Vec<&String>> {
    if k == 0 { return vec![vec![]]; }
    if k == 1 { return items.iter().map(|x| vec![x]).collect(); }
    items.iter().flat_map(|x| combos(items, k-1).into_iter().map(move |mut v| { v.insert(0, x); v })).collect()
}
