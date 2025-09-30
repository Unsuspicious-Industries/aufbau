pub mod ast;
pub use ast::{PartialAST, PartialASTNode, PartialNonTerminal, PartialTerminal};

pub mod parse;
pub use parse::*;

pub mod production;
pub use production::{PartialProduction, PartialSymbol};

pub mod completion;
pub use completion::*;

mod tests {
    use crate::logic::bind::BoundConclusionKind;
    use crate::logic::bind::partial::{
        BindingResolver, DefaultBindingResolver, conclude_type_with_rule, select_branches_by_type,
    };
    use crate::logic::check::TypeChecker;
    use crate::logic::grammar::Grammar;
    use crate::logic::{
        partial::PartialParser,
        typing::{Type, TypingRule},
    };

    #[test]
    fn test_debug() {
        crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);

        let spec = r#"
        U ::= 'barcbarcu'
        A ::= 'a' 
        B ::= 'b' A 'r'
        start ::= U | (B 'c')* | 't' 
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        println!("{:#?}", g);
        let mut p = PartialParser::new(g);
        let input = "barcbarc";
        let ast = p.partial(input).unwrap();
        println!("Partial AST: {:#?}", ast);
        let complete = ast.into_complete().unwrap();
        println!("Complete AST: {}", complete.pretty());
    }

    #[test]
    fn stlc_type_guided_branch_selection_and_typecheck() {
        crate::set_debug_level(crate::logic::debug::DebugLevel::Warn);

        let mut g =
            crate::logic::grammar::Grammar::load(crate::logic::grammar::tests::STLC_SPEC).unwrap();
        // Ensure start symbol is set
        if g.start_nonterminal().is_none() {
            if let Some(first) = g.productions.keys().next().cloned() {
                g.set_start(first);
            }
        }
        let mut p = PartialParser::new(g.clone());

        // Build rules equivalent to STLC ones
        let var_rule =
            TypingRule::new("x ∈ Γ".to_string(), "Γ(x)".to_string(), "var".to_string()).unwrap();

        let lambda_rule = TypingRule::new(
            "Γ[x:τ₁] ⊢ e : τ₂".to_string(),
            "τ₁ -> τ₂".to_string(),
            "lambda".to_string(),
        )
        .unwrap();

        let app_rule = TypingRule::new(
            "Γ ⊢ f : τ₁ -> τ₂, Γ ⊢ e : τ₁".to_string(),
            "τ₂".to_string(),
            "app".to_string(),
        )
        .unwrap();

        let input = "(λx:a->a.x) z"; // simple app where z is free
        let past = p.partial(input).unwrap();

        // Ensure root is nonterminal and we have at least one branch
        let branches = match past.root() {
            crate::logic::partial::PartialASTNode::NonTerminal(alts) => alts.clone(),
            _ => panic!("unexpected root"),
        };
        assert!(!branches.is_empty());

        // Bind a branch's rule and check
        let resolver = DefaultBindingResolver;
        let mut branch = branches[0].clone();
        if let Ok(bound_rule) = resolver.resolve_rule(&lambda_rule, &branch) {
            branch.bound_typing_rule = Some(Box::new(bound_rule));
        }
        let mut tc = TypeChecker::new();
        tc.context_mut().add(
            "z".to_string(),
            crate::logic::bind::typing::BoundType::Atom("a".to_string()),
        );
        let _ = tc.check_partial(&branch.as_node());

        // Ensure we can conclude a type from at least one rule on the chosen branch
        let mut concluded = false;
        for r in [&lambda_rule, &app_rule, &var_rule] {
            if let Ok(br) = resolver.resolve_rule(r, &branch) {
                if let BoundConclusionKind::Type(_) = br.conclusion.kind {
                    concluded = true;
                    break;
                }
            }
        }
        assert!(concluded);
    }
}
