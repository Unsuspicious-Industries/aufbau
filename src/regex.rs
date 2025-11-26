use regex::Regex as ExternalRegex;
use std::collections::{HashSet, VecDeque};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Regex {
    Empty,                          // ∅ - matches nothing
    Epsilon,                        // ε - matches empty string
    Char(char),                     // single character
    Range(char, char),              // character range inclusive
    Concat(Box<Regex>, Box<Regex>), // r1·r2
    Union(Box<Regex>, Box<Regex>),  // r1|r2
    Star(Box<Regex>),               // r*
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrefixStatus {
    Extensible(Regex), // prefix forms a complete match and can extend
    Complete,          // prefix is a complete match and cannot extend further
    Prefix(Regex),     // prefix is valid but not yet complete; derivative provided
    NoMatch,           // prefix invalid
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct CharInterval {
    start: u32,
    end: u32,
}

impl CharInterval {
    fn singleton(c: char) -> Self {
        Self {
            start: c as u32,
            end: c as u32,
        }
    }

    fn new(start: char, end: char) -> Self {
        Self {
            start: start as u32,
            end: end as u32,
        }
    }
}

impl Regex {
    // ==================== Construction from Patterns ====================

    /// Parse a regex pattern string into our internal Regex representation.
    ///
    /// Uses the `regex_syntax` crate's parser to handle the full regex syntax,
    /// then converts the resulting HIR (High-level Intermediate Representation)
    /// into our internal Regex AST.
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    ///
    /// let r = Regex::from_str("a+").unwrap();
    /// let r = Regex::from_str("[a-z]{2,5}").unwrap();
    /// ```
    pub fn from_str(pattern: &str) -> Result<Regex, String> {
        use regex_syntax::Parser;
        let hir = Parser::new().parse(pattern).map_err(|e| e.to_string())?;
        Ok(Regex::from_hir(&hir))
    }

    pub fn from_external_regex(re: &ExternalRegex) -> Result<Regex, String> {
        Self::from_str(re.as_str())
    }

    /// Alias for `from_str`.
    pub fn new(pattern: &str) -> Result<Regex, String> {
        Self::from_str(pattern)
    }

    /// Convert a regex_syntax HIR into our internal Regex representation.
    ///
    /// This is the core conversion function that maps from regex_syntax's
    /// high-level intermediate representation to our algebraic Regex type.
    ///
    /// # Supported HIR Kinds
    /// - `Empty`: Maps to Epsilon (matches empty string)
    /// - `Literal`: Single characters or byte sequences (converted to char sequences)
    /// - `Class`: Character classes (Unicode and byte ranges)
    /// - `Look`: Assertions (converted to epsilon or empty based on type)
    /// - `Concat`: Concatenation of regexes
    /// - `Alternation`: Union of regexes
    /// - `Repetition`: Zero-or-more (*), one-or-more (+), optional (?), and counted repetitions
    /// - `Capture`: Ignored, inner expression is used
    ///
    /// Unsupported constructs map to `Empty` (matches nothing).
    pub fn from_hir(hir: &regex_syntax::hir::Hir) -> Regex {
        use regex_syntax::hir::{Class, HirKind};

        fn range_to_regex(r: &regex_syntax::hir::ClassUnicodeRange) -> Regex {
            if r.start() == r.end() {
                Regex::Char(r.start())
            } else {
                Regex::Range(r.start(), r.end())
            }
        }

        match hir.kind() {
            HirKind::Empty => Regex::Epsilon,
            HirKind::Literal(l) => {
                // Literal is Box<[u8]> - convert UTF-8 bytes to string
                let bytes = l.0.as_ref();
                let value = std::str::from_utf8(bytes).expect("Invalid UTF-8 in regex literal");
                Regex::literal(value)
            }
            HirKind::Class(cls) => {
                match cls {
                    Class::Unicode(ucls) => {
                        let mut iter = ucls.iter();
                        if let Some(first) = iter.next() {
                            let mut node = range_to_regex(&first);
                            for r in iter {
                                node = Regex::Union(Box::new(node), Box::new(range_to_regex(&r)));
                            }
                            node
                        } else {
                            Regex::Empty
                        }
                    }
                    Class::Bytes(bcls) => {
                        // Convert byte class to char class
                        let mut iter = bcls.iter();
                        if let Some(first) = iter.next() {
                            let mut node = if first.start() == first.end() {
                                Regex::Char(first.start() as char)
                            } else {
                                Regex::Range(first.start() as char, first.end() as char)
                            };
                            for r in iter {
                                let r_node = if r.start() == r.end() {
                                    Regex::Char(r.start() as char)
                                } else {
                                    Regex::Range(r.start() as char, r.end() as char)
                                };
                                node = Regex::Union(Box::new(node), Box::new(r_node));
                            }
                            node
                        } else {
                            Regex::Empty
                        }
                    }
                }
            }
            HirKind::Look(_) => {
                // Lookahead/lookbehind/boundaries - map to epsilon for simplicity
                Regex::Epsilon
            }
            HirKind::Repetition(rep) => {
                let inner = Box::new(Regex::from_hir(&rep.sub));
                match rep.min {
                    0 if rep.max == Some(1) => {
                        // {0,1} = optional
                        Regex::Union(Box::new(Regex::Epsilon), inner)
                    }
                    0 if rep.max.is_none() => {
                        // {0,} = *
                        Regex::Star(inner)
                    }
                    1 if rep.max.is_none() => {
                        // {1,} = +
                        Regex::Concat(inner.clone(), Box::new(Regex::Star(inner)))
                    }
                    min if rep.max.is_none() => {
                        // {min,} = r{min} r*
                        let mut res = Regex::Epsilon;
                        for _ in 0..min {
                            res = Regex::Concat(Box::new(res), Box::new((*inner).clone()));
                        }
                        Regex::Concat(Box::new(res), Box::new(Regex::Star(inner)))
                    }
                    min if rep.max == Some(min) => {
                        // {n} = exactly n
                        let mut res = Regex::Epsilon;
                        for _ in 0..min {
                            res = Regex::Concat(Box::new(res), Box::new((*inner).clone()));
                        }
                        res
                    }
                    min => {
                        // {min,max}
                        let max = rep.max.unwrap_or(min);
                        let mut res = Regex::Epsilon;
                        for _ in 0..min {
                            res = Regex::Concat(Box::new(res), Box::new((*inner).clone()));
                        }
                        if max > min {
                            for _ in min..max {
                                res = Regex::Concat(
                                    Box::new(res),
                                    Box::new(Regex::Union(
                                        Box::new(Regex::Epsilon),
                                        Box::new((*inner).clone()),
                                    )),
                                );
                            }
                        }
                        res
                    }
                }
            }
            HirKind::Concat(xs) => xs
                .iter()
                .map(Regex::from_hir)
                .reduce(|a, b| Regex::Concat(Box::new(a), Box::new(b)))
                .unwrap_or(Regex::Epsilon),
            HirKind::Alternation(xs) => xs
                .iter()
                .map(Regex::from_hir)
                .reduce(|a, b| Regex::Union(Box::new(a), Box::new(b)))
                .unwrap_or(Regex::Empty),
            HirKind::Capture(cap) => {
                // Just use the inner expression, ignoring capture group
                Regex::from_hir(&cap.sub)
            }
        }
    }

    /// Convert the regex to a pattern string.
    ///
    /// Generates a simple string representation. Note that this may not
    /// be the most compact representation.
    pub fn to_pattern(&self) -> String {
        match self {
            Regex::Empty => String::from("(?!)"), // never matches
            Regex::Epsilon => String::new(),
            Regex::Char(c) => {
                // Escape special regex characters
                if matches!(
                    c,
                    '.' | '*'
                        | '+'
                        | '?'
                        | '|'
                        | '('
                        | ')'
                        | '['
                        | ']'
                        | '{'
                        | '}'
                        | '\\'
                        | '^'
                        | '$'
                ) {
                    format!("\\{}", c)
                } else {
                    c.to_string()
                }
            }
            Regex::Range(start, end) if start == end => Regex::Char(*start).to_pattern(),
            Regex::Range(start, end) => {
                format!("[{}-{}]", start, end)
            }
            Regex::Concat(r1, r2) => {
                format!("{}{}", r1.to_pattern(), r2.to_pattern())
            }
            Regex::Union(r1, r2) => {
                format!("({}|{})", r1.to_pattern(), r2.to_pattern())
            }
            Regex::Star(r) => match **r {
                Regex::Char(_) | Regex::Range(_, _) => format!("{}*", r.to_pattern()),
                _ => format!("({})*", r.to_pattern()),
            },
        }
    }

    // ==================== Utility Constructors ====================

    /// Create a regex that matches a literal string.
    ///
    /// Concatenates individual character matchers for each character in the string.
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::literal("hello");
    /// assert!(r.match_full("hello"));
    /// assert!(!r.match_full("world"));
    /// ```
    pub fn literal(s: &str) -> Regex {
        if s.is_empty() {
            return Regex::Epsilon;
        }
        s.chars()
            .map(Regex::Char)
            .reduce(|a, b| Regex::Concat(Box::new(a), Box::new(b)))
            .unwrap_or(Regex::Epsilon)
    }

    /// Create a regex matching zero or more repetitions of the given regex (Kleene star).
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::zero_or_more(Regex::Char('a'));
    /// // Equivalent to "a*"
    /// ```
    pub fn zero_or_more(r: Regex) -> Regex {
        Regex::Star(Box::new(r))
    }

    /// Create a regex matching one or more repetitions of the given regex.
    ///
    /// Equivalent to: `r·r*`
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::one_or_more(Regex::Char('a'));
    /// // Equivalent to "a+"
    /// ```
    pub fn one_or_more(r: Regex) -> Regex {
        Regex::Concat(Box::new(r.clone()), Box::new(Regex::Star(Box::new(r))))
    }

    /// Create a regex matching zero or one occurrence of the given regex (optional).
    ///
    /// Equivalent to: `ε|r`
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::optional(Regex::Char('a'));
    /// // Equivalent to "a?"
    /// ```
    pub fn optional(r: Regex) -> Regex {
        Regex::Union(Box::new(Regex::Epsilon), Box::new(r))
    }

    /// Create a regex matching exactly `n` repetitions of the given regex.
    ///
    /// Equivalent to: `r·r·r...·r` (n times)
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::exactly(Regex::Char('a'), 3);
    /// // Equivalent to "aaa" or "a{3}"
    /// ```
    pub fn exactly(r: Regex, n: usize) -> Regex {
        if n == 0 {
            return Regex::Epsilon;
        }
        (0..n)
            .map(|_| r.clone())
            .reduce(|a, b| Regex::Concat(Box::new(a), Box::new(b)))
            .unwrap_or(Regex::Epsilon)
    }

    /// Create a regex matching at least `min` repetitions of the given regex.
    ///
    /// Equivalent to: `r{min}·r*`
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::at_least(Regex::Char('a'), 2);
    /// // Equivalent to "a{2,}" or "aa+"
    /// ```
    pub fn at_least(r: Regex, min: usize) -> Regex {
        let min_part = Self::exactly(r.clone(), min);
        Regex::Concat(Box::new(min_part), Box::new(Regex::Star(Box::new(r))))
    }

    /// Create a regex matching between `min` and `max` repetitions (inclusive).
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::between(Regex::Char('a'), 2, 4);
    /// // Equivalent to "a{2,4}"
    /// ```
    pub fn between(r: Regex, min: usize, max: usize) -> Regex {
        if max < min {
            return Regex::Empty;
        }
        if min == max {
            return Self::exactly(r, min);
        }

        // Build: r{min} followed by 0 to (max-min) optional r's
        let required = Self::exactly(r.clone(), min);
        let optional_count = max - min;

        // Build (r|ε) repeated (max-min) times
        let optional_part = (0..optional_count)
            .map(|_| Self::optional(r.clone()))
            .reduce(|a, b| Regex::Concat(Box::new(a), Box::new(b)))
            .unwrap_or(Regex::Epsilon);

        Regex::Concat(Box::new(required), Box::new(optional_part))
    }

    /// Create a regex matching any character in the given string.
    ///
    /// Equivalent to a character class like `[abc]`.
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::any_of("abc");
    /// // Equivalent to "[abc]" or "a|b|c"
    /// ```
    pub fn any_of(chars: &str) -> Regex {
        chars
            .chars()
            .map(Regex::Char)
            .reduce(|a, b| Regex::Union(Box::new(a), Box::new(b)))
            .unwrap_or(Regex::Empty)
    }

    /// Create a regex matching any character.
    ///
    /// Matches the full Unicode range.
    pub fn any_char() -> Regex {
        Regex::Range('\u{0000}', '\u{10FFFF}')
    }

    /// Concatenate two regexes.
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::concat(Regex::Char('a'), Regex::Char('b'));
    /// // Equivalent to "ab"
    /// ```
    pub fn concat(r1: Regex, r2: Regex) -> Regex {
        Regex::Concat(Box::new(r1), Box::new(r2))
    }

    /// Create a union (alternation) of two regexes.
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::union(Regex::Char('a'), Regex::Char('b'));
    /// // Equivalent to "a|b"
    /// ```
    pub fn union(r1: Regex, r2: Regex) -> Regex {
        Regex::Union(Box::new(r1), Box::new(r2))
    }

    /// Concatenate multiple regexes in sequence.
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::concat_many(vec![
    ///     Regex::Char('a'),
    ///     Regex::Char('b'),
    ///     Regex::Char('c')
    /// ]);
    /// // Equivalent to "abc"
    /// ```
    pub fn concat_many<I>(regexes: I) -> Regex
    where
        I: IntoIterator<Item = Regex>,
    {
        regexes
            .into_iter()
            .reduce(|a, b| Regex::Concat(Box::new(a), Box::new(b)))
            .unwrap_or(Regex::Epsilon)
    }

    /// Create a union (alternation) of multiple regexes.
    ///
    /// # Examples
    /// ```
    /// use p7::regex::Regex;
    /// let r = Regex::union_many(vec![
    ///     Regex::Char('a'),
    ///     Regex::Char('b'),
    ///     Regex::Char('c')
    /// ]);
    /// // Equivalent to "a|b|c"
    /// ```
    pub fn union_many<I>(regexes: I) -> Regex
    where
        I: IntoIterator<Item = Regex>,
    {
        regexes
            .into_iter()
            .reduce(|a, b| Regex::Union(Box::new(a), Box::new(b)))
            .unwrap_or(Regex::Empty)
    }

    // ==================== Common Pattern Constructors ====================

    /// Create a regex matching any digit (0-9).
    pub fn digit() -> Regex {
        Regex::Range('0', '9')
    }

    /// Create a regex matching any lowercase letter (a-z).
    pub fn lowercase() -> Regex {
        Regex::Range('a', 'z')
    }

    /// Create a regex matching any uppercase letter (A-Z).
    pub fn uppercase() -> Regex {
        Regex::Range('A', 'Z')
    }

    /// Create a regex matching any letter (a-z or A-Z).
    pub fn alpha() -> Regex {
        Regex::Union(Box::new(Regex::lowercase()), Box::new(Regex::uppercase()))
    }

    /// Create a regex matching any alphanumeric character (a-z, A-Z, or 0-9).
    pub fn alphanumeric() -> Regex {
        Regex::Union(Box::new(Regex::alpha()), Box::new(Regex::digit()))
    }

    /// Create a regex matching whitespace characters (space, tab, newline, etc.).
    pub fn whitespace() -> Regex {
        Regex::any_of(" \t\n\r")
    }

    /// Create a regex matching word characters (letters, digits, and underscore).
    pub fn word() -> Regex {
        Regex::Union(Box::new(Regex::alphanumeric()), Box::new(Regex::Char('_')))
    }

    // ==================== Analysis & Operations ====================

    pub fn simplify(&self) -> Regex {
        self._simplify()
    }

    fn _simplify(&self) -> Regex {
        match self {
            Regex::Concat(r1, r2) => {
                let s1 = r1._simplify();
                let s2 = r2._simplify();
                match (&s1, &s2) {
                    (Regex::Empty, _) | (_, Regex::Empty) => Regex::Empty,
                    (Regex::Epsilon, _) => s2,
                    (_, Regex::Epsilon) => s1,
                    _ => Regex::Concat(Box::new(s1), Box::new(s2)),
                }
            }
            Regex::Union(r1, r2) => {
                let s1 = r1._simplify();
                let s2 = r2._simplify();
                match (&s1, &s2) {
                    (Regex::Empty, _) => s2,
                    (_, Regex::Empty) => s1,
                    _ if s1 == s2 => s1,
                    _ => Regex::Union(Box::new(s1), Box::new(s2)),
                }
            }
            _ => self.clone(),
        }
    }

    /// Structural equivalence after simplification.
    pub fn equiv(&self, other: &Regex) -> bool {
        self.simplify() == other.simplify()
    }

    /// Brzozowski derivative of the regex w.r.t. a character.
    fn char_derivative(&self, c: char) -> Regex {
        use Regex::*;
        match self {
            Empty => Empty,
            Epsilon => Empty,
            Char(ch) => {
                if *ch == c {
                    Epsilon
                } else {
                    Empty
                }
            }
            Range(start, end) => {
                if c >= *start && c <= *end {
                    Epsilon
                } else {
                    Empty
                }
            }
            Union(a, b) => Regex::Union(
                Box::new((**a).char_derivative(c)),
                Box::new((**b).char_derivative(c)),
            ),
            Concat(a, b) => {
                let da = a.char_derivative(c);
                if a.nullable() {
                    Regex::Union(
                        Box::new(Regex::Concat(Box::new(da), Box::new((**b).clone()))),
                        Box::new(b.char_derivative(c)),
                    )
                } else {
                    Regex::Concat(Box::new(da), Box::new((**b).clone()))
                }
            }
            Star(r) => Regex::Concat(
                Box::new((**r).char_derivative(c)),
                Box::new(Star(r.clone())),
            ),
        }
    }

    pub fn derivative(&self, s: &str) -> Regex {
        s.chars()
            .fold(self.clone(), |acc, c| acc.char_derivative(c))
    }

    pub fn prefix_match(&self, word: &str) -> PrefixStatus {
        let deriv = self.derivative(word).simplify();

        if deriv.empty() {
            PrefixStatus::NoMatch
        } else if deriv.null() {
            PrefixStatus::Complete
        } else if deriv.nullable() {
            PrefixStatus::Extensible(deriv)
        } else {
            PrefixStatus::Prefix(deriv)
        }
    }

    /// Produce an arbitrarily chosen example string accepted by this regex.
    pub fn example(&self) -> Option<String> {
        use Regex::*;
        match self {
            Empty => None,
            Epsilon => Some(String::new()),
            Char(c) => Some(c.to_string()),
            Concat(a, b) => Some(format!("{}{}", a.example()?, b.example()?)),
            Union(a, b) => a.example().or_else(|| b.example()),
            Star(_) => Some(String::new()),
            Range(start, _) => Some(start.to_string()),
        }
    }

    pub fn nullable(&self) -> bool {
        use Regex::*;
        match self {
            Empty => false,
            Epsilon => true,
            Char(_) => false,
            Concat(a, b) => a.nullable() && b.nullable(),
            Union(a, b) => a.nullable() || b.nullable(),
            Star(_) => true,
            Range(..) => false,
        }
    }

    pub fn null(&self) -> bool {
        use Regex::*;
        match self {
            Epsilon => true,
            _ => false,
        }
    }

    pub fn empty(&self) -> bool {
        match self {
            Regex::Empty => true,
            Regex::Epsilon => false,
            Regex::Char(_) => false,
            Regex::Union(r1, r2) => r1.empty() && r2.empty(),
            Regex::Concat(r1, r2) => r1.empty() || r2.empty(),
            Regex::Star(_) => false,
            Regex::Range(..) => false,
        }
    }

    /// True if the entire `input` string matches the regex.
    pub fn match_full(&self, input: &str) -> bool {
        self.derivative(input).nullable()
    }

    pub fn contains_regex(&self, other: &Regex) -> bool {
        // r1 contains r2 if for every string matched by r2, r1 also matches it.
        // Equivalently, r1 contains r2 if the intersection of r2 and the complement of r1 is empty.
        let mut queue: VecDeque<(Regex, Regex)> = VecDeque::new();
        let mut visited: HashSet<(Regex, Regex)> = HashSet::new();

        queue.push_back((self.simplify(), other.simplify()));

        while let Some((super_r, sub_r)) = queue.pop_front() {
            if !visited.insert((super_r.clone(), sub_r.clone())) {
                continue;
            }

            if sub_r.nullable() && !super_r.nullable() {
                return false;
            }

            if sub_r.empty() {
                continue;
            }

            let intervals = sub_r.leading_char_intervals();
            if intervals.is_empty() {
                continue;
            }

            for interval in intervals {
                if let Some(c) = char::from_u32(interval.start) {
                    let next_super = super_r.char_derivative(c).simplify();
                    let next_sub = sub_r.char_derivative(c).simplify();

                    if next_sub.empty() {
                        continue;
                    }

                    queue.push_back((next_super, next_sub));
                }
            }
        }

        true
    }

    // very inefficient substring search using regex matching
    // we'll do proper automata-based search later
    pub fn find(&self, input: &str) -> Option<(usize, usize)> {
        // Find the longest match starting at position 0
        // This is important for correct tokenization - we want to match the full identifier "main",
        // not just the first character "m"
        // We need to use character indices, not byte indices, to handle Unicode correctly
        let char_indices: Vec<usize> = input
            .char_indices()
            .map(|(i, _)| i)
            .chain(std::iter::once(input.len()))
            .collect();

        for i in (1..char_indices.len()).rev() {
            let end_byte = char_indices[i];
            let substr = &input[0..end_byte];
            if self.match_full(substr) {
                return Some((0, end_byte));
            }
        }
        None
    }

    fn leading_char_intervals(&self) -> Vec<CharInterval> {
        let mut intervals = Vec::new();
        self.collect_leading_char_intervals(&mut intervals);
        merge_char_intervals(intervals)
    }

    fn collect_leading_char_intervals(&self, acc: &mut Vec<CharInterval>) {
        use Regex::*;
        match self {
            Empty | Epsilon => {}
            Char(c) => acc.push(CharInterval::singleton(*c)),
            Range(start, end) => acc.push(CharInterval::new(*start, *end)),
            Union(left, right) => {
                left.collect_leading_char_intervals(acc);
                right.collect_leading_char_intervals(acc);
            }
            Concat(first, second) => {
                if first.empty() {
                    return;
                }

                first.collect_leading_char_intervals(acc);
                if first.nullable() {
                    second.collect_leading_char_intervals(acc);
                }
            }
            Star(inner) => inner.collect_leading_char_intervals(acc),
        }
    }
}

fn merge_char_intervals(mut intervals: Vec<CharInterval>) -> Vec<CharInterval> {
    if intervals.is_empty() {
        return intervals;
    }

    intervals.sort_by(|a, b| a.start.cmp(&b.start).then_with(|| a.end.cmp(&b.end)));

    let mut merged: Vec<CharInterval> = Vec::with_capacity(intervals.len());
    for interval in intervals {
        if let Some(last) = merged.last_mut() {
            if interval.start <= last.end {
                if interval.end > last.end {
                    last.end = interval.end;
                }
            } else {
                merged.push(interval);
            }
        } else {
            merged.push(interval);
        }
    }

    merged
}

#[cfg(test)]
mod tests {
    use super::{PrefixStatus, Regex};

    #[test]
    fn test_nullable_and_empty() {
        let cases = vec![
            ("", true, false),   // ε
            ("a", false, false), // single char
            ("a*", true, false),
            ("a|b", false, false),
            ("(ab)*", true, false),
        ];
        for (pattern, expected_nullable, expected_empty) in cases {
            let r = Regex::new(pattern).expect("valid regex pattern");
            assert_eq!(
                r.nullable(),
                expected_nullable,
                "nullable failed for pattern '{pattern}'"
            );
            assert_eq!(
                r.empty(),
                expected_empty,
                "empty failed for pattern '{pattern}'"
            );
        }
    }

    #[test]
    fn test_match_full() {
        let cases = vec![
            ("a", "a", true),
            ("a", "b", false),
            ("a|b", "a", true),
            ("a|b", "b", true),
            ("ab", "ab", true),
            ("ab", "a", false),
            ("a*", "", true),
            ("a*", "aaa", true),
            ("a*", "b", false),
            ("(ab)*", "abab", true),
        ];
        for (pattern, input, expected) in cases {
            let r = Regex::new(pattern).expect("valid regex pattern");
            assert_eq!(
                r.match_full(input),
                expected,
                "match_full failed for pattern '{pattern}' with input '{input}'"
            );
        }
    }

    #[test]
    fn test_example_matches() {
        let patterns = vec![
            "",
            "a",
            "a*",
            "a|b",
            "ab",
            "(ab)*",
            "(hello|world)*!",
            "[0-9]+",
            "[a-z]{2,5}",
            "(cat|dog|bird)s?",
            "a{2,4}b",
        ];
        for pattern in patterns {
            let r = Regex::new(pattern).expect("valid regex pattern");
            if let Some(example) = r.example() {
                assert!(
                    r.match_full(&example),
                    "example '{}' does not match pattern '{}'",
                    example,
                    pattern
                );
            } else {
                // Only Empty should return None
                assert!(
                    r.empty(),
                    "pattern '{}' returned no example but is not empty",
                    pattern
                );
            }
        }
    }

    #[derive(Debug)]
    enum Expected {
        Full,
        No,
        Prefix(&'static str),
        Ext(&'static str),
    }

    #[test]
    fn test_prefix_match_complex() {
        use Expected::*;
        let cases = vec![
            // Basic Kleene star patterns
            ("a*b", "", Prefix("a*b")),
            ("a*b", "a", Prefix("a*b")),
            ("a*b", "aa", Prefix("a*b")),
            ("a*b", "aaa", Prefix("a*b")),
            ("a*b", "ab", Full),
            ("a*b", "aab", Full),
            ("a*b", "ac", No),
            ("a*b", "b", Full),
            // Plus (one or more) patterns
            ("a+b", "", Prefix("a+b")),
            ("a+b", "a", Prefix("a*b")),
            ("a+b", "aa", Prefix("a*b")),
            ("a+b", "ab", Full),
            ("a+b", "aab", Full),
            ("a+b", "b", No),
            // Optional patterns
            ("a?b", "", Prefix("a?b")),
            ("a?b", "a", Prefix("b")),
            ("a?b", "ab", Full),
            ("a?b", "b", Full),
            ("a?b", "aa", No),
            ("a?b", "c", No),
            // Alternation patterns
            ("(ab|cd)*ef", "", Prefix("(ab|cd)*ef")),
            ("(ab|cd)*ef", "ab", Prefix("(ab|cd)*ef")),
            ("(ab|cd)*ef", "cd", Prefix("(ab|cd)*ef")),
            ("(ab|cd)*ef", "abcd", Prefix("(ab|cd)*ef")),
            ("(ab|cd)*ef", "cdab", Prefix("(ab|cd)*ef")),
            ("(ab|cd)*ef", "abef", Full),
            ("(ab|cd)*ef", "cdef", Full),
            ("(ab|cd)*ef", "ef", Full),
            ("(ab|cd)*ef", "abcdef", Full),
            ("(ab|cd)*ef", "gh", No),
            ("(ab|cd)*ef", "ae", No),
            // Simple concatenation
            ("abc", "", Prefix("abc")),
            ("abc", "a", Prefix("bc")),
            ("abc", "ab", Prefix("c")),
            ("abc", "abc", Full),
            ("abc", "abcd", No),
            ("abc", "abd", No),
            // Simple alternation
            ("a|b", "", Prefix("a|b")),
            ("a|b", "a", Full),
            ("a|b", "b", Full),
            ("a|b", "c", No),
            ("a|b", "ab", No),
            // Character class patterns
            ("[0-9]+", "", Prefix("[0-9]+")),
            ("[0-9]+", "1", Ext("[0-9]*")),
            ("[0-9]+", "12", Ext("[0-9]*")),
            ("[0-9]+", "123", Ext("[0-9]*")),
            ("[0-9]+", "1234", Ext("[0-9]*")),
            ("[0-9]+", "12345", Ext("[0-9]*")),
            ("[0-9]+", "a", No),
            // Mixed patterns
            ("(hello|world)*!", "", Prefix("(hello|world)*!")),
            ("(hello|world)*!", "hello", Prefix("(hello|world)*!")),
            ("(hello|world)*!", "world", Prefix("(hello|world)*!")),
            ("(hello|world)*!", "helloworld", Prefix("(hello|world)*!")),
            ("(hello|world)*!", "!", Full),
            ("(hello|world)*!", "hello!", Full),
            ("(hello|world)*!", "helloworld!", Full),
            ("(hello|world)*!", "hi", No),
            // Nested star patterns
            ("(a*b)*c", "", Prefix("(a*b)*c")),
            ("(a*b)*c", "b", Prefix("(a*b)*c")),
            ("(a*b)*c", "ab", Prefix("(a*b)*c")),
            ("(a*b)*c", "c", Full),
            ("(a*b)*c", "bc", Full),
            ("(a*b)*c", "abc", Full),
            ("(a*b)*c", "d", No),
            // Quantifier patterns
            ("a{2,4}b", "", Prefix("a{2,4}b")),
            ("a{2,4}b", "a", Prefix("a{1,3}b")),
            ("a{2,4}b", "aa", Prefix("a{0,2}b")),
            ("a{2,4}b", "aab", Full),
            ("a{2,4}b", "aaab", Full),
            ("a{2,4}b", "aaaab", Full),
            ("a{2,4}b", "aaaaab", No),
            // Multiple alternatives with concat
            ("(cat|dog|bird)s?", "", Prefix("(cat|dog|bird)s?")),
            ("(cat|dog|bird)s?", "cat", Ext("s?")), // derivative is s?
            ("(cat|dog|bird)s?", "cats", Full),
            ("(cat|dog|bird)s?", "dog", Ext("s?")), // derivative is s?
            ("(cat|dog|bird)s?", "dogs", Full),
            ("(cat|dog|bird)s?", "bird", Ext("s?")), // derivative is s?
            ("(cat|dog|bird)s?", "birds", Full),
            ("(cat|dog|bird)s?", "fish", No),
            // Complex real-world-like patterns
            ("[a-z]+@[a-z]+", "", Prefix("[a-z]+@[a-z]+")),
            ("[a-z]+@[a-z]+", "user", Prefix("[a-z]*@[a-z]+")), // derivative is [a-z]*@[a-z]+
            ("[a-z]+@[a-z]+", "user@", Prefix("[a-z]+")),
            ("[a-z]+@[a-z]+", "user@domain", Ext("[a-z]*")), // derivative is [a-z]*
            ("[a-z]+@[a-z]+", "userdomain.com", No),
            // Edge cases with empty matches
            ("a*", "", Ext("a*")),
            ("a*", "a", Ext("a*")),
            ("a*", "aa", Ext("a*")),
            ("a*", "aaa", Ext("a*")),
            ("a*", "b", No),
            ("(a|b)*", "", Ext("(a|b)*")),
            ("(a|b)*", "a", Ext("(a|b)*")),
            ("(a|b)*", "b", Ext("(a|b)*")),
            ("(a|b)*", "aa", Ext("(a|b)*")),
            ("(a|b)*", "bb", Ext("(a|b)*")),
            ("(a|b)*", "abab", Ext("(a|b)*")),
            ("(a|b)*", "c", No),
        ];

        for (pattern, input, expected) in cases {
            let r = Regex::new(pattern).expect("valid regex pattern");
            let status = r.prefix_match(input);
            match (status, expected) {
                (PrefixStatus::Complete, Full) => {}
                (PrefixStatus::NoMatch, No) => {}
                (PrefixStatus::Prefix(deriv), Prefix(exp_deriv)) => {
                    let exp_r = Regex::new(exp_deriv).expect("expected derivative parse");
                    assert!(
                        deriv.equiv(&exp_r),
                        "Derivative mismatch for pattern '{pattern}' and input '{input}'.\nGot: {}\nExpected: {}",
                        deriv.to_pattern(),
                        exp_deriv
                    );
                }
                (PrefixStatus::Extensible(deriv), Ext(exp_deriv)) => {
                    let exp_r = Regex::new(exp_deriv).expect("expected derivative parse");
                    assert!(
                        deriv.equiv(&exp_r),
                        "Derivative mismatch for pattern '{pattern}' and input '{input}'.\nGot: {}\nExpected: {}",
                        deriv.to_pattern(),
                        exp_deriv
                    );
                }
                (s, e) => {
                    let status_name = match s {
                        PrefixStatus::Complete => "FullMatch",
                        PrefixStatus::NoMatch => "NoMatch",
                        PrefixStatus::Prefix(_) => "Prefix",
                        PrefixStatus::Extensible(_) => "Extensible",
                    };
                    let expected_name = match e {
                        Full => "FullMatch",
                        No => "NoMatch",
                        Prefix(_) => "Prefix",
                        Ext(_) => "Extensible",
                    };
                    panic!(
                        "Unexpected match status {status_name} for pattern '{pattern}' with input '{input}' (expected {expected_name})"
                    );
                }
            }
        }
    }

    #[test]
    fn test_utility_constructors() {
        // Test literal
        let lit = Regex::literal("hello");
        assert!(lit.match_full("hello"));
        assert!(!lit.match_full("world"));

        // Test repetitions
        let zero_or_more = Regex::zero_or_more(Regex::Char('a'));
        assert!(zero_or_more.match_full(""));
        assert!(zero_or_more.match_full("aaa"));

        let one_or_more = Regex::one_or_more(Regex::Char('b'));
        assert!(!one_or_more.match_full(""));
        assert!(one_or_more.match_full("bbb"));

        let optional = Regex::optional(Regex::Char('c'));
        assert!(optional.match_full(""));
        assert!(optional.match_full("c"));
        assert!(!optional.match_full("cc"));

        // Test exactly
        let exactly_3 = Regex::exactly(Regex::Char('x'), 3);
        assert!(exactly_3.match_full("xxx"));
        assert!(!exactly_3.match_full("xx"));
        assert!(!exactly_3.match_full("xxxx"));

        // Test common patterns
        let digit = Regex::digit();
        assert!(digit.match_full("5"));
        assert!(!digit.match_full("a"));

        let alpha = Regex::alpha();
        assert!(alpha.match_full("a"));
        assert!(alpha.match_full("Z"));
        assert!(!alpha.match_full("5"));

        // Test any_of
        let vowels = Regex::any_of("aeiou");
        assert!(vowels.match_full("a"));
        assert!(vowels.match_full("e"));
        assert!(!vowels.match_full("b"));

        // Test concat_many
        let abc = Regex::concat_many(vec![Regex::Char('a'), Regex::Char('b'), Regex::Char('c')]);
        assert!(abc.match_full("abc"));

        // Test union_many
        let choices = Regex::union_many(vec![Regex::Char('x'), Regex::Char('y'), Regex::Char('z')]);
        assert!(choices.match_full("x"));
        assert!(choices.match_full("y"));
        assert!(!choices.match_full("a"));
    }

    #[test]
    fn test_character_classes() {
        // Test digit range
        let digits = Regex::from_str("[0-9]").unwrap();
        for c in '0'..='9' {
            assert!(
                digits.match_full(&c.to_string()),
                "digit '{}' should match [0-9]",
                c
            );
        }
        assert!(!digits.match_full("a"));

        // Test lowercase range
        let lower = Regex::from_str("[a-z]").unwrap();
        assert!(lower.match_full("a"));
        assert!(lower.match_full("m"));
        assert!(lower.match_full("z"));
        assert!(!lower.match_full("A"));
        assert!(!lower.match_full("5"));

        // Test uppercase range
        let upper = Regex::from_str("[A-Z]").unwrap();
        assert!(upper.match_full("A"));
        assert!(upper.match_full("M"));
        assert!(upper.match_full("Z"));
        assert!(!upper.match_full("a"));

        // Test multiple ranges
        let alphanum = Regex::from_str("[a-zA-Z0-9]").unwrap();
        assert!(alphanum.match_full("a"));
        assert!(alphanum.match_full("Z"));
        assert!(alphanum.match_full("5"));
        assert!(!alphanum.match_full("_"));
        assert!(!alphanum.match_full("!"));

        // Test negation (should not match)
        let not_digit = Regex::from_str("[^0-9]").unwrap();
        assert!(!not_digit.match_full("5"));
        assert!(not_digit.match_full("a"));

        // Test specific character set
        let vowels = Regex::from_str("[aeiou]").unwrap();
        assert!(vowels.match_full("a"));
        assert!(vowels.match_full("e"));
        assert!(vowels.match_full("i"));
        assert!(!vowels.match_full("b"));
        assert!(!vowels.match_full("z"));
    }

    #[test]
    fn test_repetition_quantifiers() {
        // Test * (zero or more)
        let star = Regex::from_str("a*").unwrap();
        assert!(star.match_full(""));
        assert!(star.match_full("a"));
        assert!(star.match_full("aaaa"));
        assert!(!star.match_full("b"));

        // Test + (one or more)
        let plus = Regex::from_str("b+").unwrap();
        assert!(!plus.match_full(""));
        assert!(plus.match_full("b"));
        assert!(plus.match_full("bbbb"));
        assert!(!plus.match_full("a"));

        // Test ? (optional)
        let question = Regex::from_str("c?").unwrap();
        assert!(question.match_full(""));
        assert!(question.match_full("c"));
        assert!(!question.match_full("cc"));

        // Test {n} (exactly n)
        let exact_3 = Regex::from_str("d{3}").unwrap();
        assert!(!exact_3.match_full("dd"));
        assert!(exact_3.match_full("ddd"));
        assert!(!exact_3.match_full("dddd"));

        // Test {n,} (at least n)
        let at_least_2 = Regex::from_str("e{2,}").unwrap();
        assert!(!at_least_2.match_full("e"));
        assert!(at_least_2.match_full("ee"));
        assert!(at_least_2.match_full("eeee"));

        // Test {n,m} (between n and m)
        let between_2_4 = Regex::from_str("f{2,4}").unwrap();
        assert!(!between_2_4.match_full("f"));
        assert!(between_2_4.match_full("ff"));
        assert!(between_2_4.match_full("fff"));
        assert!(between_2_4.match_full("ffff"));
        assert!(!between_2_4.match_full("fffff"));
    }

    #[test]
    fn test_alternation() {
        // Simple alternation
        let simple = Regex::from_str("a|b").unwrap();
        assert!(simple.match_full("a"));
        assert!(simple.match_full("b"));
        assert!(!simple.match_full("c"));
        assert!(!simple.match_full("ab"));

        // Multi-character alternation
        let multi = Regex::from_str("hello|world").unwrap();
        assert!(multi.match_full("hello"));
        assert!(multi.match_full("world"));
        assert!(!multi.match_full("helloworld"));
        assert!(!multi.match_full("hi"));

        // Multiple alternatives
        let triple = Regex::from_str("cat|dog|bird").unwrap();
        assert!(triple.match_full("cat"));
        assert!(triple.match_full("dog"));
        assert!(triple.match_full("bird"));
        assert!(!triple.match_full("fish"));

        // Nested alternation
        let nested = Regex::from_str("(a|b)(c|d)").unwrap();
        assert!(nested.match_full("ac"));
        assert!(nested.match_full("ad"));
        assert!(nested.match_full("bc"));
        assert!(nested.match_full("bd"));
        assert!(!nested.match_full("ab"));
        assert!(!nested.match_full("cd"));
    }

    #[test]
    fn test_concatenation() {
        // Simple concat
        let simple = Regex::from_str("abc").unwrap();
        assert!(simple.match_full("abc"));
        assert!(!simple.match_full("ab"));
        assert!(!simple.match_full("abcd"));

        // Concat with quantifiers
        let with_quant = Regex::from_str("a+b*c?").unwrap();
        assert!(with_quant.match_full("a"));
        assert!(with_quant.match_full("abc"));
        assert!(with_quant.match_full("aabbc"));
        assert!(with_quant.match_full("aaac"));
        assert!(!with_quant.match_full("bc"));

        // Concat with groups
        let with_groups = Regex::from_str("(ab)(cd)").unwrap();
        assert!(with_groups.match_full("abcd"));
        assert!(!with_groups.match_full("ab"));
        assert!(!with_groups.match_full("cd"));
    }

    #[test]
    fn test_capture_groups() {
        // Capture groups should be ignored, but inner expression preserved
        let grouped = Regex::from_str("(hello)").unwrap();
        assert!(grouped.match_full("hello"));
        assert!(!grouped.match_full("world"));

        // Multiple groups
        let multi_group = Regex::from_str("(a)(b)(c)").unwrap();
        assert!(multi_group.match_full("abc"));

        // Nested groups
        let nested = Regex::from_str("((ab)c)").unwrap();
        assert!(nested.match_full("abc"));
        assert!(!nested.match_full("ab"));

        // Groups with alternation
        let group_alt = Regex::from_str("(a|b)c").unwrap();
        assert!(group_alt.match_full("ac"));
        assert!(group_alt.match_full("bc"));
        assert!(!group_alt.match_full("c"));
    }

    #[test]
    fn test_complex_patterns() {
        // Email-like pattern (simplified)
        let email = Regex::from_str("[a-z]+@[a-z]+").unwrap();
        assert!(email.match_full("user@domain"));
        assert!(email.match_full("test@example"));
        assert!(!email.match_full("@domain"));
        assert!(!email.match_full("user@"));

        // Phone number pattern
        let phone = Regex::from_str("[0-9]{3}-[0-9]{4}").unwrap();
        assert!(phone.match_full("123-4567"));
        assert!(!phone.match_full("12-34567"));
        assert!(!phone.match_full("1234567"));

        // URL pattern (simplified)
        let url = Regex::from_str("(http|https)://[a-z]+").unwrap();
        assert!(url.match_full("http://example"));
        assert!(url.match_full("https://test"));
        assert!(!url.match_full("ftp://example"));

        // Identifier pattern
        let identifier = Regex::from_str("[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
        assert!(identifier.match_full("variable"));
        assert!(identifier.match_full("_private"));
        assert!(identifier.match_full("var123"));
        assert!(!identifier.match_full("123var"));

        // Hex color
        let hex_color = Regex::from_str("#[0-9a-fA-F]{6}").unwrap();
        assert!(hex_color.match_full("#ff0000"));
        assert!(hex_color.match_full("#ABCDEF"));
        assert!(!hex_color.match_full("#ff00"));
        assert!(!hex_color.match_full("ff0000"));
    }

    #[test]
    fn test_edge_cases() {
        // Empty pattern
        let empty = Regex::from_str("").unwrap();
        assert!(empty.match_full(""));
        assert!(!empty.match_full("a"));

        // Single character
        let single = Regex::from_str("x").unwrap();
        assert!(single.match_full("x"));
        assert!(!single.match_full(""));
        assert!(!single.match_full("xx"));

        // Optional everything
        let all_optional = Regex::from_str("a?b?c?").unwrap();
        assert!(all_optional.match_full(""));
        assert!(all_optional.match_full("a"));
        assert!(all_optional.match_full("ab"));
        assert!(all_optional.match_full("abc"));
        assert!(all_optional.match_full("ac"));
        assert!(!all_optional.match_full("abcd"));

        // Many stars
        let many_stars = Regex::from_str("a*b*c*").unwrap();
        assert!(many_stars.match_full(""));
        assert!(many_stars.match_full("aaa"));
        assert!(many_stars.match_full("bbb"));
        assert!(many_stars.match_full("ccc"));
        assert!(many_stars.match_full("abc"));
        assert!(many_stars.match_full("aabbcc"));
    }

    #[test]
    fn test_any_char_dot() {
        // Dot should match any character
        let dot = Regex::from_str(".").unwrap();
        assert!(dot.match_full("a"));
        assert!(dot.match_full("Z"));
        assert!(dot.match_full("5"));
        assert!(dot.match_full("!"));
        assert!(!dot.match_full(""));
        assert!(!dot.match_full("ab"));

        // Multiple dots
        let dots = Regex::from_str("...").unwrap();
        assert!(dots.match_full("abc"));
        assert!(dots.match_full("123"));
        assert!(!dots.match_full("ab"));
        assert!(!dots.match_full("abcd"));

        // Dot with quantifiers
        let dot_star = Regex::from_str(".*").unwrap();
        assert!(dot_star.match_full(""));
        assert!(dot_star.match_full("anything"));
        assert!(dot_star.match_full("!@#$%"));
    }

    #[test]
    fn test_escaped_characters() {
        // Test escaped metacharacters
        let escaped_dot = Regex::from_str(r"\.").unwrap();
        assert!(escaped_dot.match_full("."));
        assert!(!escaped_dot.match_full("a"));

        let escaped_star = Regex::from_str(r"\*").unwrap();
        assert!(escaped_star.match_full("*"));
        assert!(!escaped_star.match_full("a"));

        let escaped_plus = Regex::from_str(r"\+").unwrap();
        assert!(escaped_plus.match_full("+"));
        assert!(!escaped_plus.match_full("a"));

        // Backslash itself
        let backslash = Regex::from_str(r"\\").unwrap();
        assert!(backslash.match_full("\\"));
    }

    #[test]
    fn test_word_boundaries_and_anchors() {
        // Word boundaries and anchors are converted to epsilon
        // They don't affect matching in our simple model
        let with_anchor = Regex::from_str("^hello$").unwrap();
        assert!(with_anchor.match_full("hello"));

        let with_boundary = Regex::from_str(r"\bhello\b").unwrap();
        assert!(with_boundary.match_full("hello"));
    }

    #[test]
    fn test_from_hir_all_features() {
        // Comprehensive test of from_hir with various patterns
        let test_cases = vec![
            // Basic
            ("a", vec!["a"], vec!["b", ""]),
            ("abc", vec!["abc"], vec!["ab", "abcd"]),
            // Alternation
            ("a|b|c", vec!["a", "b", "c"], vec!["d", "ab"]),
            // Quantifiers
            ("a*", vec!["", "a", "aaa"], vec!["b"]),
            ("a+", vec!["a", "aaa"], vec!["", "b"]),
            ("a?", vec!["", "a"], vec!["aa"]),
            ("a{3}", vec!["aaa"], vec!["aa", "aaaa"]),
            ("a{2,4}", vec!["aa", "aaa", "aaaa"], vec!["a", "aaaaa"]),
            // Character classes
            ("[abc]", vec!["a", "b", "c"], vec!["d", "ab"]),
            ("[0-9]", vec!["0", "5", "9"], vec!["a", "10"]),
            ("[a-z]", vec!["a", "m", "z"], vec!["A", "0"]),
            // Combinations
            ("(a|b)+", vec!["a", "b", "ab", "ba", "aabb"], vec!["", "c"]),
            ("[0-9]{2,3}", vec!["12", "123"], vec!["1", "1234"]),
            (
                "(hello|world)*",
                vec!["", "hello", "world", "helloworld"],
                vec!["hi"],
            ),
        ];

        for (pattern, should_match, should_not_match) in test_cases {
            let regex = Regex::from_str(pattern).unwrap();

            for input in should_match {
                assert!(
                    regex.match_full(input),
                    "Pattern '{}' should match '{}' but didn't",
                    pattern,
                    input
                );
            }

            for input in should_not_match {
                assert!(
                    !regex.match_full(input),
                    "Pattern '{}' should not match '{}' but did",
                    pattern,
                    input
                );
            }
        }
    }

    #[test]
    fn test_utility_functions_comprehensive() {
        // Test at_least
        let at_least_2 = Regex::at_least(Regex::Char('a'), 2);
        assert!(!at_least_2.match_full("a"));
        assert!(at_least_2.match_full("aa"));
        assert!(at_least_2.match_full("aaaa"));

        // Test between
        let between = Regex::between(Regex::Char('b'), 2, 4);
        assert!(!between.match_full("b"));
        assert!(between.match_full("bb"));
        assert!(between.match_full("bbb"));
        assert!(between.match_full("bbbb"));
        assert!(!between.match_full("bbbbb"));

        // Test whitespace
        let ws = Regex::whitespace();
        assert!(ws.match_full(" "));
        assert!(ws.match_full("\t"));
        assert!(ws.match_full("\n"));
        assert!(!ws.match_full("a"));

        // Test word
        let word = Regex::word();
        assert!(word.match_full("a"));
        assert!(word.match_full("Z"));
        assert!(word.match_full("5"));
        assert!(word.match_full("_"));
        assert!(!word.match_full("!"));

        // Test alphanumeric
        let alnum = Regex::alphanumeric();
        assert!(alnum.match_full("a"));
        assert!(alnum.match_full("Z"));
        assert!(alnum.match_full("5"));
        assert!(!alnum.match_full("_"));
        assert!(!alnum.match_full("!"));
    }

    #[test]
    fn test_to_pattern() {
        // Test conversion to pattern string
        let r = Regex::Char('a');
        assert_eq!(r.to_pattern(), "a");

        let r = Regex::Range('a', 'z');
        assert_eq!(r.to_pattern(), "[a-z]");

        let r = Regex::Star(Box::new(Regex::Char('a')));
        assert_eq!(r.to_pattern(), "a*");

        let r = Regex::Union(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        assert_eq!(r.to_pattern(), "(a|b)");

        let r = Regex::Concat(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        assert_eq!(r.to_pattern(), "ab");

        // Test escaping
        let r = Regex::Char('*');
        assert_eq!(r.to_pattern(), "\\*");
    }
}
