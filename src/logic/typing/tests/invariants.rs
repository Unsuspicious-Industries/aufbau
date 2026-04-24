use crate::logic::synth::Synthesizer;
use crate::logic::typing::{Context, Type, Unifier};
use crate::validation::parseable::check_all_prefixes_parseable;
use proptest::prelude::*;

fn ctx_of(pairs: &[(&str, &str)]) -> Context {
    let mut ctx = Context::new();
    for (name, ty) in pairs {
        ctx.add(name.to_string(), Type::parse_raw(ty).unwrap());
    }
    ctx
}

fn stlc_chain_context(arg_count: usize) -> Vec<(String, String)> {
    let type_names: Vec<String> = (0..=arg_count).map(|i| format!("T{}", i)).collect();
    let mut f_ty = type_names[arg_count].clone();
    for i in (0..arg_count).rev() {
        f_ty = format!("{}->{}", type_names[i], f_ty);
    }

    let mut pairs = vec![("f".to_string(), f_ty)];
    for i in 0..arg_count {
        let name = match i {
            0 => "x".to_string(),
            1 => "y".to_string(),
            2 => "z".to_string(),
            3 => "u".to_string(),
            4 => "v".to_string(),
            _ => format!("a{}", i),
        };
        pairs.push((name, type_names[i].clone()));
    }
    pairs
}

fn stlc_chain_input(arg_count: usize) -> String {
    let names: Vec<String> = stlc_chain_context(arg_count)
        .into_iter()
        .map(|(name, _)| name)
        .collect();
    names.join(" ")
}

fn ctx_from_owned(pairs: &[(String, String)]) -> Context {
    let mut ctx = Context::new();
    for (name, ty) in pairs {
        ctx.add(name.clone(), Type::parse_raw(ty).unwrap());
    }
    ctx
}

#[test]
fn unifier_rejects_direct_occurs_bind() {
    let mut u = Unifier::new();
    let ty = Type::parse("?A -> 'Int'").unwrap();
    assert!(u.bind("A", &ty).is_fail());
}

#[test]
fn ruleless_multichild_root_has_any_type() {
    let grammar = crate::logic::grammar::Grammar::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Pair ::= Variable Variable
        Start ::= Pair

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar, "x y");
    let ast = synth
        .parse_with(&ctx_of(&[("x", "'X'"), ("y", "'Y'")]))
        .unwrap();
    let arena = ast.arena();
    assert!(ast.root_ids().iter().any(|&id| {
        arena
            .node(id)
            .and_then(|node| synth.runtime().type_of(node.ty))
            == Some(Type::Any)
    }));
}

#[test]
fn transparent_unary_ruleless_root_inherits_child_type() {
    let grammar = crate::logic::grammar::Grammar::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Wrap ::= Variable
        Start ::= Wrap

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar, "x");
    let ast = synth.parse_with(&ctx_of(&[("x", "'X'")])).unwrap();
    let arena = ast.arena();
    let grammar = synth.grammar().clone();
    let root_types: Vec<_> = ast
        .root_ids()
        .iter()
        .filter_map(|&id| {
            arena.node(id).map(|node| {
                (
                    id,
                    grammar.nt(node.nt).map(|s| s.to_string()),
                    node.status,
                    synth.runtime().type_of(node.ty),
                    node.span.start,
                    node.span.end,
                )
            })
        })
        .collect();
    assert!(
        ast.root_ids().iter().any(|&id| {
            arena
                .node(id)
                .and_then(|node| synth.runtime().type_of(node.ty))
                == Some(Type::parse("'X'").unwrap())
        }),
        "root types: {:?}",
        root_types
    );
}

#[test]
fn mixed_nonterminal_unary_alt_inherits_child_type() {
    let grammar = crate::logic::grammar::Grammar::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Atom ::= Variable | '(' Variable ')'
        Start ::= Atom

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar, "x");
    let ast = synth.parse_with(&ctx_of(&[("x", "'X'")])).unwrap();
    let arena = ast.arena();
    assert!(ast.root_ids().iter().any(|&id| {
        arena
            .node(id)
            .and_then(|node| synth.runtime().type_of(node.ty))
            == Some(Type::parse("'X'").unwrap())
    }));
}

#[test]
fn transparent_delimited_wrapper_inherits_child_type() {
    let grammar = crate::logic::grammar::Grammar::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Group ::= '(' Variable ')'
        Start ::= Group

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar, "(x)");
    let ast = synth.parse_with(&ctx_of(&[("x", "'X'")])).unwrap();
    let arena = ast.arena();
    assert!(ast.root_ids().iter().any(|&id| {
        arena
            .node(id)
            .and_then(|node| synth.runtime().type_of(node.ty))
            == Some(Type::parse("'X'").unwrap())
    }));
}

#[test]
fn unresolved_meta_is_not_exported_as_final_chain_type() {
    let grammar =
        crate::logic::grammar::Grammar::load(include_str!("../../../../examples/stlc.auf"))
            .unwrap();
    let mut synth = Synthesizer::new(grammar, "f x y");
    let ast = synth
        .parse_with(&ctx_of(&[("f", "A->B->C"), ("x", "A"), ("y", "B")]))
        .unwrap();
    let arena = ast.arena();
    assert!(ast.root_ids().iter().any(|&id| {
        arena
            .node(id)
            .and_then(|node| synth.runtime().type_of(node.ty))
            == Some(Type::parse_raw("C").unwrap())
    }));
}

#[test]
fn empty_variable_prefix_is_accepted_as_unknown_not_contradiction() {
    let grammar = crate::logic::grammar::Grammar::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Start ::= Variable

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar, "");
    let ast = synth.parse_with(&ctx_of(&[("x", "'X'")])).unwrap();
    assert!(ast.is_complete() || !ast.root_ids().is_empty());
}

#[test]
fn closed_int_left_operand_does_not_reopen_for_float_operator() {
    let grammar =
        crate::logic::grammar::Grammar::load(include_str!("../../../../examples/fun.auf")).unwrap();
    let mut synth = Synthesizer::new(grammar, "10 /. 2.0");
    assert!(synth.parse_with(&Context::new()).is_err());
}

#[test]
fn closed_parenthesized_int_expr_does_not_reopen_for_float_operator() {
    let grammar =
        crate::logic::grammar::Grammar::load(include_str!("../../../../examples/fun.auf")).unwrap();
    let mut synth = Synthesizer::new(grammar, "((1 + 2) * (3 + 4)) /. 2.0");
    assert!(synth.parse_with(&Context::new()).is_err());
}

proptest! {
    #[test]
    fn prop_bridge_wrapper_inherits_child_type(name in "[a-z]{1,6}") {
        let grammar = crate::logic::grammar::Grammar::load(
            r#"
            Identifier ::= /[a-z]+/
            Variable(var) ::= Identifier[x]
            Wrap ::= Variable
            Start ::= Wrap

            x ∈ Γ
            ----------- (var)
            Γ(x)
            "#,
        ).unwrap();
        let mut synth = Synthesizer::new(grammar, &name);
        let ast = synth.parse_with(&ctx_of(&[(&name, "'X'")])).unwrap();
        let arena = ast.arena();
        let ok = ast.root_ids().iter().any(|&id| {
            arena.node(id)
                .and_then(|node| synth.runtime().type_of(node.ty))
                == Some(Type::parse("'X'").unwrap())
        });
        prop_assert!(ok);
    }

    #[test]
    fn prop_ruleless_multichild_stays_any(a in "[a-z]{1,4}", b in "[a-z]{1,4}") {
        prop_assume!(a != b);
        let grammar = crate::logic::grammar::Grammar::load(
            r#"
            Identifier ::= /[a-z]+/
            Variable(var) ::= Identifier[x]
            Pair ::= Variable Variable
            Start ::= Pair

            x ∈ Γ
            ----------- (var)
            Γ(x)
            "#,
        ).unwrap();
        let mut synth = Synthesizer::new(grammar, format!("{} {}", a, b));
        let ast = synth.parse_with(&ctx_of(&[(&a, "'L'"), (&b, "'R'")])).unwrap();
        let arena = ast.arena();
        let ok = ast.root_ids().iter().any(|&id| {
            arena.node(id)
                .and_then(|node| synth.runtime().type_of(node.ty))
                == Some(Type::Any)
        });
        prop_assert!(ok);
    }

    #[test]
    fn prop_generated_stlc_chains_parse_and_resolve(arg_count in 1usize..6) {
        let grammar = crate::logic::grammar::Grammar::load(include_str!("../../../../examples/stlc.auf")).unwrap();
        let input = stlc_chain_input(arg_count);
        let ctx_pairs = stlc_chain_context(arg_count);
        let ctx = ctx_from_owned(&ctx_pairs);
        let expected = Type::parse_raw(&format!("T{}", arg_count)).unwrap();

        let mut synth = Synthesizer::new(grammar, &input);
        let ast = synth.parse_with(&ctx).unwrap();
        let arena = ast.arena();
        let final_ok = ast.root_ids().iter().any(|&id| {
            arena.node(id)
                .and_then(|node| synth.runtime().type_of(node.ty))
                == Some(expected.clone())
        });
        prop_assert!(final_ok);
    }

    #[test]
    fn prop_generated_stlc_chain_prefixes_parse(arg_count in 1usize..6) {
        let mut grammar = crate::logic::grammar::Grammar::load(include_str!("../../../../examples/stlc.auf")).unwrap();
        let input = stlc_chain_input(arg_count);
        let ctx_pairs = stlc_chain_context(arg_count);
        let ctx = ctx_from_owned(&ctx_pairs);

        let result = check_all_prefixes_parseable(&mut grammar, &input, &ctx);
        let ok = matches!(result, crate::validation::parseable::ParseResult::Pass { .. });
        prop_assert!(ok);
    }
}
