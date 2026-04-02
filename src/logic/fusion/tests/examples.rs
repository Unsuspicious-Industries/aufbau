use crate::logic::fusion::{
    BindingValue, ChildRef, CtxId, NodeStatus, NtId, PathId, ProdId, TransitionError, TypeId,
    TypedParser, TypingContextSummary, TypingRuntime, TypingState, display,
};
use crate::logic::grammar::Segment;
use crate::logic::typing::{Context, Type};
use crate::regex::Regex;

#[derive(Clone, Debug, Default)]
struct ExampleTyping;

impl TypingRuntime for ExampleTyping {
    fn enter_nonterminal(&self, _nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState> {
        vec![TypingState {
            ctx: summary.ctx,
            expected: summary.expected,
            inferred: Some(TypeId(1)),
            path: summary.path,
            bindings: Vec::new(),
        }]
    }

    fn prepare_child(
        &self,
        _prod: ProdId,
        _child_idx: usize,
        _binding: Option<&str>,
        state: &TypingState,
        _parsed_children: &[TypingState],
    ) -> Result<TypingState, TransitionError> {
        Ok(state.clone())
    }

    fn descend(
        &self,
        state: &TypingState,
        path: PathId,
        binding: Option<&str>,
    ) -> Result<TypingState, TransitionError> {
        let mut next = state.clone();
        next.path = Some(path);
        if let Some(name) = binding {
            next.bindings.push(BindingValue {
                name: name.to_string(),
                path,
                value: None,
                ty: None,
            });
        }
        Ok(next)
    }

    fn consume_terminal(
        &self,
        state: &TypingState,
        _regex: &Regex,
        _segment: Option<&Segment>,
    ) -> Result<TypingState, TransitionError> {
        Ok(state.clone())
    }

    fn finish_production(
        &self,
        prod: ProdId,
        state: &TypingState,
        _children: &[TypingState],
        _status: NodeStatus,
    ) -> Result<TypingState, TransitionError> {
        Ok(TypingState {
            ctx: state.ctx,
            expected: state.expected,
            inferred: Some(TypeId(prod.0 + 1)),
            path: state.path,
            bindings: state.bindings.clone(),
        })
    }
}

fn norm(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[test]
fn example_fun_literal_prefix() {
    let grammar = crate::testing::load_example_grammar("fun");
    let mut parser = TypedParser::new(grammar, ExampleTyping);
    let state = parser.parse("1", CtxId(0)).unwrap();

    assert!(!state.roots.is_empty());
}

#[test]
fn example_stlc_lambda_prefix_stays_typed() {
    let grammar = crate::testing::load_example_grammar("stlc");
    let mut parser = TypedParser::new(grammar, ExampleTyping).with_max_depth(24);
    let state = parser.parse("λx:A.x", CtxId(0)).unwrap();

    assert!(!state.roots.is_empty());
}

#[test]
fn rule_runtime_supports_partial_nested_stlc_lambda() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let grammar = crate::testing::load_example_grammar("stlc");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(48);
    let state = parser.parse("λx:A.λ", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn rule_runtime_matches_old_literal_success() {
    let grammar = crate::testing::load_example_grammar("fun");
    let segments = grammar.tokenize("1").unwrap();

    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("1", CtxId(0)).unwrap();

    assert!(!state.roots.is_empty());
    assert_eq!(
        norm(&display::render_node_text(
            &parser,
            state.roots[0],
            &segments
        )),
        "1"
    );
}

#[test]
fn rule_runtime_matches_zero_premise_dsl() {
    let grammar = crate::logic::grammar::Grammar::load(
        "Num(num) ::= /[0-9]+/
start ::= Num

----------- (num)
Int",
    )
    .unwrap();
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("42", CtxId(0)).unwrap();
    let node = parser.arena().node(state.roots[0]).unwrap();

    assert_eq!(node.status, NodeStatus::Complete);
}

#[test]
fn rule_runtime_supports_context_lookup() {
    let grammar = crate::logic::grammar::Grammar::load(
        "Identifier ::= /[a-z]+/
Variable(var) ::= Identifier[x]
start ::= Variable

x ∈ Γ
----------- (var)
Γ(x)",
    )
    .unwrap();
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut ctx = Context::new();
    ctx.add("foo".to_string(), Type::Raw("Int".to_string()));
    let ctx_id = runtime.intern_context(ctx);
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("foo", ctx_id).unwrap();
    assert_eq!(state.roots.len(), 1);
}

#[test]
fn rule_runtime_supports_context_extension_shape() {
    let grammar = crate::logic::grammar::Grammar::load(
        "Identifier ::= /[a-z]+/
Integer(int) ::= /[0-9]+/
Variable(var) ::= Identifier[x]
Let(let) ::= 'let' Identifier[name] '=' Integer[value] ';' Variable[body]
start ::= Let

----------- (int)
'Int'

x ∈ Γ
----------- (var)
Γ(x)

Γ ⊢ value : 'Int', Γ[name:'Int'] ⊢ body : ?R
------------------------------------- (let)
?R",
    )
    .unwrap();
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("let foo = 1 ; foo", CtxId(0)).unwrap();
    assert_eq!(state.roots.len(), 1);
}

#[test]
fn rule_runtime_supports_fun_identity_lambda() {
    let grammar = crate::testing::load_example_grammar("fun");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("(x: Int) => x", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn rule_runtime_supports_fun_annotated_let() {
    let grammar = crate::testing::load_example_grammar("fun");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("let x : Int = 1 ; x", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn rule_runtime_supports_fun_float_operator() {
    let grammar = crate::testing::load_example_grammar("fun");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("1.0 +. 2.5", CtxId(0)).unwrap();
    assert_eq!(state.roots.len(), 1);
}

#[test]
fn rule_runtime_supports_imp_program() {
    let grammar = crate::testing::load_example_grammar("imp");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(48);
    let state = parser.parse("{ let x: Int = 1; }", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn stlc_application_chain_repro_f_x_y() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let grammar = crate::testing::load_example_grammar("stlc");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let mut ctx = Context::new();
    ctx.add(
        "f".to_string(),
        crate::logic::typing::Type::parse_raw("A->B->C").unwrap(),
    );
    ctx.add(
        "x".to_string(),
        crate::logic::typing::Type::parse_raw("A").unwrap(),
    );
    ctx.add(
        "y".to_string(),
        crate::logic::typing::Type::parse_raw("B").unwrap(),
    );
    let ctx_id = runtime.intern_context(ctx);
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(62);
    let state = parser.parse("f x y", ctx_id).unwrap();
    assert_eq!(state.roots.len(), 1);
    let node = parser.arena().node(state.roots[0]).unwrap();
    assert_eq!(node.status, NodeStatus::Complete);
}

#[test]
fn fun_type_error_int_plus_float_should_not_be_complete() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let grammar = crate::testing::load_example_grammar("fun");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let parser = TypedParser::new(grammar, runtime.clone()).with_max_depth(40);
    let meta = crate::logic::fusion::MetaTypedParser::new(parser)
        .with_start_depth(8)
        .with_max_depth(40)
        .with_depth_factor(1.5);
    let (state, _depth, arena) = meta.parse_with_arena("1 + 2.0", ctx_id).unwrap();
    for id in &state.roots {
        if let Some(n) = arena.node(*id) {
            eprintln!("root {:?} status={:?} span={:?}", id, n.status, n.span);
            if let Some(alts) = arena.alts_for(*id) {
                for (i, alt) in alts.iter().enumerate() {
                    let mut child_summ = Vec::new();
                    for ch in &alt.children {
                        match ch {
                            ChildRef::Node(nid) => {
                                let st = arena
                                    .node(*nid)
                                    .map(|nn| format!("{:?}:{:?}", nid, nn.status))
                                    .unwrap_or_else(|| format!("{:?}:<missing>", nid));
                                child_summ.push(st);
                            }
                            ChildRef::Terminal(t) => {
                                child_summ.push(format!(
                                    "tok[{}..{}]{}",
                                    t.start,
                                    t.end,
                                    if t.complete { "" } else { "?" }
                                ));
                            }
                        }
                    }
                    eprintln!(
                        "  alt[{i}] prod={:?} rule={:?} children={:?}",
                        alt.prod,
                        runtime.production_rule_name(alt.prod),
                        child_summ
                    );
                }
            }
        }
    }
    let has_complete_root = state.roots.iter().any(|id| {
        arena
            .node(*id)
            .is_some_and(|n| matches!(n.status, NodeStatus::Complete))
    });
    assert!(
        !has_complete_root,
        "expected no complete typed root for type-error expression"
    );
}
