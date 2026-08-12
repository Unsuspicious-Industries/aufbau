//! The shared corpora, certified from Rust.
//!
//! `corpora/<lang>/{valid,invalid,beyond}.txt` is the differential
//! certification input. The OCaml harness (`ocaml/cert.ml`) runs it against a
//! real type checker; this runs the same files against the engine directly, so
//! the two certificates are statements about one corpus rather than two
//! independent sets of examples that happen to look alike.
//!
//! Languages are discovered from the directory, not listed here: a new
//! `corpora/<lang>/` with a matching `examples/<lang>.auf` is certified without
//! editing this file.

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use crate::grammar::SPG;
    use crate::typing::{Context, TypingSynth};

    fn repo_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    }

    /// Non-empty, non-`#` lines. Matches `Certify.load_corpus` in `ocaml/certify.ml`,
    /// so both harnesses see the same programs.
    fn load(path: &Path) -> Vec<String> {
        let Ok(text) = std::fs::read_to_string(path) else {
            return Vec::new();
        };
        text.lines()
            .map(str::trim)
            .filter(|l| !l.is_empty() && !l.starts_with('#'))
            .map(ToString::to_string)
            .collect()
    }

    /// `(language, grammar)` for every corpus directory with a matching grammar.
    fn corpora() -> Vec<(String, SPG)> {
        let root = repo_root();
        let dir = root.join("corpora");
        let mut out = Vec::new();
        let entries = std::fs::read_dir(&dir).unwrap_or_else(|e| panic!("{}: {e}", dir.display()));
        for entry in entries {
            let path = entry.unwrap().path();
            if !path.is_dir() {
                continue;
            }
            let lang = path.file_name().unwrap().to_string_lossy().into_owned();
            let grammar = root.join("examples").join(format!("{lang}.auf"));
            assert!(
                grammar.exists(),
                "corpora/{lang} has no examples/{lang}.auf to certify against"
            );
            let src = std::fs::read_to_string(&grammar).unwrap();
            out.push((
                lang.clone(),
                SPG::load(&src).unwrap_or_else(|e| panic!("{lang}.auf: {e}")),
            ));
        }
        assert!(!out.is_empty(), "no corpora found under {}", dir.display());
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }

    /// Whether the whole program types: a complete parse under the empty context.
    /// This is the `"typed"` of the Python `status()` and the `aufbau_ok` of the
    /// OCaml harness.
    fn types(grammar: &SPG, program: &str) -> bool {
        TypingSynth::new(grammar.clone(), program)
            .parse_with(&Context::new())
            .is_ok_and(|ast| ast.is_complete())
    }

    fn check(kind: &str, want_typed: bool) {
        let mut failures = Vec::new();
        let mut total = 0usize;
        for (lang, grammar) in corpora() {
            let path = repo_root().join("corpora").join(&lang).join(kind);
            for program in load(&path) {
                total += 1;
                if types(&grammar, &program) != want_typed {
                    failures.push(format!("  {lang}/{kind}: {program}"));
                }
            }
        }
        assert!(
            failures.is_empty(),
            "{} of {total} {kind} programs disagreed (want typed={want_typed}):\n{}",
            failures.len(),
            failures.join("\n")
        );
    }

    /// Every `valid` program types. A failure here means the fragment is too
    /// narrow: it rejects something the corpus says it must accept.
    #[test]
    fn valid_programs_type() {
        check("valid.txt", true);
    }

    /// No `invalid` program types. A failure here is unsoundness — the engine
    /// accepting something the corpus says is ill-typed.
    #[test]
    fn invalid_programs_do_not_type() {
        check("invalid.txt", false);
    }

    /// `beyond` programs are valid in the real language but outside the
    /// certified fragment, so the engine must *not* type them. If one starts
    /// typing, the fragment grew and the program belongs in `valid.txt`.
    #[test]
    fn beyond_programs_do_not_type() {
        check("beyond.txt", false);
    }

    /// The corpus is the shared input, so it has to be non-trivial in both
    /// directions for the certificate above to mean anything.
    #[test]
    fn every_corpus_has_both_directions() {
        for (lang, _) in corpora() {
            let dir = repo_root().join("corpora").join(&lang);
            for kind in ["valid.txt", "invalid.txt"] {
                assert!(
                    !load(&dir.join(kind)).is_empty(),
                    "corpora/{lang}/{kind} is empty"
                );
            }
        }
    }
}
