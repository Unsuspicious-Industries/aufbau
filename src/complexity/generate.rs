use crate::{logic::grammar::{self, Grammar, Production, Symbol}, regex::Regex};

// generate complete or partial inputs from a grammar
struct Generator {
    grammar: Grammar,
}

impl Generator {
    fn new(grammar: Grammar) -> Self {
        Self { grammar }
    }

    // Generate a complete input string from the grammar
    // m is max recusrion
    // l is level
    fn generate_rec(&self, p: Production, d: usize, m:usize, l:usize) -> Vec<Regex> {
        if l > m {
            return vec![];
        }
        let b = match &p[d] {
            Symbol::Terminal { regex, .. } => vec![regex.clone()],
            Symbol::Nonterminal { name, .. } => {
                // unwrap is not safe but error means illegal grammar
                let productions = self.grammar.production(&name).unwrap();
                productions
                    .iter()
                    .map(|prod| self.generate_rec(prod.clone(), 0, m, l + 1))
                    .flatten()
                    .collect::<Vec<Regex>>()
            }
        };
        if d >= p.len() - 1 {
            return b;
        }
        let mut results: Vec<Regex> = vec![];
        for s in b {
            let suffixes = self.generate_rec(p.clone(), d + 1, m, l);
            for suffix in suffixes {
                // deduplicate
                let rex = Regex::concat_many(vec![s.clone(), Regex::literal(" "), suffix.clone()]);
                if !results.contains(&rex) {
                    results.push(rex);
                }
            }
        }
        results
    }

    fn generate(&self, m: usize) -> Vec<Regex> {
        let mut results = vec![];
        if let Some(prods) = self.grammar.production(self.grammar.start().unwrap()) {
            for prod in prods {
                results.extend(self.generate_rec(prod.clone(), 0, m, 0));
            }
        }
        results
    }
}

#[cfg(test)]
mod tests {
    use crate::testing::load_example_grammar;

    use super::*;

    #[test]
    fn test_generator() {
        let grammar = Grammar::load(r#"
        START ::= 'x'
        "#).unwrap();
        let mut g = Generator::new(grammar);
        let inputs = g.generate(3);
        for input in inputs {
            println!("{}", input);
        }
    }
    #[test]
    fn test_generator_stlc() {
        let grammar= load_example_grammar("stlc");
        let mut g = Generator::new(grammar);
        let inputs = g.generate(10);
        for input in inputs {
            println!("{}", input.example().unwrap());
        }
    }
}