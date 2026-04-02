/// Levenshtein distance with O(min(m,n)) space
pub fn levenshtein(s1: &str, s2: &str) -> usize {
    let (short, long) = {
        let (a, b) = (
            s1.chars().collect::<Vec<_>>(),
            s2.chars().collect::<Vec<_>>(),
        );
        if a.len() <= b.len() { (a, b) } else { (b, a) }
    };

    let (m, n) = (short.len(), long.len());
    if m == 0 {
        return n;
    }

    let mut prev = (0..=m).collect::<Vec<_>>();
    let mut curr = vec![0; m + 1];

    for j in 1..=n {
        curr[0] = j;
        for i in 1..=m {
            let cost = if short[i - 1] == long[j - 1] { 0 } else { 1 };
            curr[i] = (prev[i] + 1).min(curr[i - 1] + 1).min(prev[i - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[m]
}
