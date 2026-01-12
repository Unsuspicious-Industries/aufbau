//! Example string generation.

use super::Regex;

pub fn example(r: &Regex) -> Option<String> {
    let examples = examples(r, 1);
    examples.first().cloned()
}

pub fn examples(r: &Regex, n: usize) -> Vec<String> {
    let mut out = Vec::with_capacity(n);
    collect(r, n, &mut out);
    out
}

fn collect(r: &Regex, n: usize, out: &mut Vec<String>) {
    if out.len() >= n {
        return;
    }
    match r {
        Regex::Empty => {}
        Regex::Epsilon => push_unique(out, String::new()),
        Regex::Char(c) => push_unique(out, c.to_string()),
        Regex::Range(lo, hi) => {
            for c in (*lo as u32)..=(*hi as u32) {
                if out.len() >= n {
                    break;
                }
                if let Some(ch) = char::from_u32(c) {
                    push_unique(out, ch.to_string());
                }
            }
        }
        Regex::Union(a, b) => {
            let (mut va, mut vb) = (vec![], vec![]);
            collect(a, n, &mut va);
            collect(b, n, &mut vb);
            let mut i = 0;
            while out.len() < n && (i < va.len() || i < vb.len()) {
                if i < va.len() {
                    push_unique(out, va[i].clone());
                }
                if out.len() < n && i < vb.len() {
                    push_unique(out, vb[i].clone());
                }
                i += 1;
            }
        }
        Regex::Concat(a, b) => {
            let k = ((n as f64).sqrt().ceil() as usize).max(2);
            let (mut va, mut vb) = (vec![], vec![]);
            collect(a, k, &mut va);
            collect(b, k, &mut vb);
            'done: for x in &va {
                for y in &vb {
                    if out.len() >= n {
                        break 'done;
                    }
                    push_unique(out, format!("{x}{y}"));
                }
            }
        }
        Regex::Star(inner) => {
            push_unique(out, String::new());
            let mut sub = vec![];
            collect(inner, n, &mut sub);
            for rep in 1..=3 {
                if out.len() >= n {
                    break;
                }
                for combo in combos(&sub, rep) {
                    if out.len() >= n {
                        break;
                    }
                    push_unique(out, combo.into_iter().cloned().collect());
                }
            }
        }
    }
}

fn push_unique(out: &mut Vec<String>, s: String) {
    if !out.contains(&s) {
        out.push(s);
    }
}

fn combos(items: &[String], k: usize) -> Vec<Vec<&String>> {
    if k == 0 {
        return vec![vec![]];
    }
    if k == 1 {
        return items.iter().map(|x| vec![x]).collect();
    }
    items
        .iter()
        .flat_map(|x| {
            combos(items, k - 1).into_iter().map(move |mut v| {
                v.insert(0, x);
                v
            })
        })
        .collect()
}
