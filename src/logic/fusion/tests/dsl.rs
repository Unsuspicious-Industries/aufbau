use crate::logic::fusion::{
    BindingValue, ChildRef, CtxId, MetaTypedParser, NodeStatus, NtId, PathId, ProdId, RuleRuntime,
    TransitionError, TypeId, TypedParser, TypingContextSummary, TypingRuntime, TypingState,
};
use crate::logic::grammar::{Grammar, Segment};
use crate::regex::Regex;
use crate::set_debug_level;

#[derive(Clone, Debug, Default)]
struct StubTyping;

impl TypingRuntime for StubTyping {
    fn enter_nonterminal(&self, _nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState> {
        vec![TypingState {
            ctx: summary.ctx,
            expected: summary.expected,
            inferred: Some(TypeId(0)),
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
            inferred: Some(TypeId(prod.0)),
            path: state.path,
            bindings: state.bindings.clone(),
        })
    }
}

#[derive(Clone, Debug, Default)]
struct RejectingTyping;

impl TypingRuntime for RejectingTyping {
    fn enter_nonterminal(&self, _nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState> {
        vec![TypingState {
            ctx: summary.ctx,
            expected: summary.expected,
            inferred: Some(TypeId(0)),
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
        _prod: ProdId,
        _state: &TypingState,
        _children: &[TypingState],
        _status: NodeStatus,
    ) -> Result<TypingState, TransitionError> {
        Err(TransitionError::Rejected)
    }
}

#[derive(Clone, Debug, Default)]
struct PathPruningTyping;

impl TypingRuntime for PathPruningTyping {
    fn enter_nonterminal(&self, _nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState> {
        vec![TypingState {
            ctx: summary.ctx,
            expected: summary.expected,
            inferred: Some(TypeId(0)),
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
        if matches!(binding, Some("bad")) {
            return Err(TransitionError::Rejected);
        }
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

#[derive(Clone, Debug, Default)]
struct DepthTyping;

impl TypingRuntime for DepthTyping {
    fn enter_nonterminal(&self, _nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState> {
        vec![TypingState {
            ctx: summary.ctx,
            expected: summary.expected,
            inferred: Some(TypeId(0)),
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
        _binding: Option<&str>,
    ) -> Result<TypingState, TransitionError> {
        let mut next = state.clone();
        next.path = Some(path);
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

#[test]
fn typed_parser_starts_empty() {
    let grammar = Grammar::load("start ::= 'x'").unwrap();
    let parser = TypedParser::new(grammar, StubTyping);

    assert_eq!(parser.frontier().len(), 0);
    assert_eq!(parser.arena().node_count(), 0);
    assert_eq!(parser.arena().alt_count(), 0);
}

#[test]
fn typed_parser_seeds_prefix_state() {
    let grammar = Grammar::load("start ::= 'x'").unwrap();
    let parser = TypedParser::new(grammar, StubTyping);
    let state = parser.seed_state(3, CtxId(0));

    assert_eq!(state.input_len, 3);
    assert!(state.roots.is_empty());
    assert!(state.frontier.is_empty());
}

#[test]
fn dsl_complete_literal() {
    let grammar = Grammar::load("start ::= 'x'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let state = parser.parse("x", CtxId(0)).unwrap();

    let node = parser.arena().node(state.roots[0]).unwrap();
    assert_eq!(node.status, NodeStatus::Complete);
}

#[test]
fn dsl_partial_root() {
    let grammar = Grammar::load("start ::= 'x' 'y'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let state = parser.parse("x", CtxId(0)).unwrap();

    let node = parser.arena().node(state.roots[0]).unwrap();
    assert_eq!(node.status, NodeStatus::Partial);
}

#[test]
fn dsl_incremental_advance() {
    set_debug_level(crate::DebugLevel::Trace);
    let grammar = Grammar::load("start ::= 'x' 'y'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let prefix = parser.parse("x", CtxId(0)).unwrap();
    let advanced = parser.advance(&prefix, "x y", CtxId(0)).unwrap();

    let node = parser.arena().node(advanced.roots[0]).unwrap();
    assert_eq!(node.status, NodeStatus::Complete);
}

#[test]
fn dsl_rejects_dead_branches() {
    let grammar = Grammar::load("start ::= 'x'").unwrap();
    let mut parser = TypedParser::new(grammar, RejectingTyping);
    let err = parser.parse("x", CtxId(0)).unwrap_err();

    assert!(!err.depth.hit_depth_limit);
}

#[test]
fn dsl_reports_too_deep() {
    let grammar = Grammar::load(
        "Atom ::= 'x'
Expr ::= Expr Atom | Atom
start ::= Expr",
    )
    .unwrap();
    let mut parser = TypedParser::new(grammar, DepthTyping).with_max_depth(1);
    let err = parser.parse("x x x", CtxId(0)).unwrap_err();

    assert!(err.depth.hit_depth_limit);
    assert!(err.depth.depth_failures > 0);
}

#[test]
fn dsl_records_terminal_children() {
    let grammar = Grammar::load("start ::= 'x'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let state = parser.parse("x", CtxId(0)).unwrap();
    let alts = parser.arena().alts_for(state.roots[0]).unwrap();

    assert!(matches!(alts[0].children[0], ChildRef::Terminal(_)));
}

#[test]
fn dsl_prunes_bad_binding_paths() {
    let grammar = Grammar::load(
        "Left ::= 'x'
Right ::= 'x'
start ::= Left[bad] | Right[good]",
    )
    .unwrap();
    let mut parser = TypedParser::new(grammar, PathPruningTyping);
    let state = parser.parse("x", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
    let alts = parser.arena().alts_for(state.roots[0]).unwrap();
    assert_eq!(alts.len(), 1);
}

#[test]
fn dsl_preserves_multiple_bindings() {
    let grammar = Grammar::load(
        "Left ::= 'x'
Right ::= 'y'
start ::= Left[a] Right[b]",
    )
    .unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let state = parser.parse("x y", CtxId(0)).unwrap();
    let alts = parser.arena().alts_for(state.roots[0]).unwrap();

    assert_eq!(alts[0].children.len(), 2);
}

#[test]
fn dsl_rule_runtime_supports_lambda_style_extension() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let grammar = Grammar::load(
        "Identifier ::= /[a-z]+/
Variable(var) ::= Identifier[x]
Lambda(lam) ::= 'fn' Identifier[param] '=>' Variable[body]
start ::= Lambda

x ∈ Γ
----------- (var)
Γ(x)

Γ[param:'Int'] ⊢ body : ?R
------------------------- (lam)
'Int' -> ?R",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("fn x => x", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn dsl_rule_runtime_keeps_partial_lambda_prefix() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let grammar = Grammar::load(
        "TypeName ::= 'Int'
Type ::= TypeName
Identifier ::= /[a-z]+/
Variable(var) ::= Identifier[x]
Lambda(lam) ::= 'fn' Identifier[param] ':' Type[τ] '=>' Variable[body]
start ::= Lambda

x ∈ Γ
----------- (var)
Γ(x)

Γ[param:τ] ⊢ body : ?R
---------------------- (lam)
τ -> ?R",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("fn", CtxId(0)).unwrap();
    let node = parser.arena().node(state.roots[0]).unwrap();

    assert_eq!(node.status, NodeStatus::Partial);
}

#[test]
fn dsl_rule_runtime_uses_bound_type_annotations() {
    let grammar = Grammar::load(
        "TypeName ::= 'Int'
Type ::= TypeName
Identifier ::= /[a-z]+/
Variable(var) ::= Identifier[x]
Lambda(lam) ::= 'fn' Identifier[param] ':' Type[τ] '=>' Variable[body]
start ::= Lambda

x ∈ Γ
----------- (var)
Γ(x)

Γ[param:τ] ⊢ body : ?R
---------------------- (lam)
τ -> ?R",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("fn x : Int => x", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn dsl_rule_runtime_rejects_mismatched_annotation() {
    let grammar = Grammar::load(
        "TypeName ::= 'Int' | 'Bool'
Type ::= TypeName
Identifier ::= /[a-z]+/
Integer(int) ::= /[0-9]+/
Boolean(bool) ::= 'true'
Let(let) ::= 'let' Identifier[name] ':' Type[τ] '=' Boolean[value] ';' Integer[body]
start ::= Let

----------- (int)
'Int'

----------- (bool)
'Bool'

Γ ⊢ value : τ, Γ[name:τ] ⊢ body : ?R
------------------------------------- (let)
?R",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);

    assert!(parser.parse("let x : Int = true ; 1", CtxId(0)).is_err());
}

#[test]
fn dsl_rule_runtime_unifies_child_types() {
    let grammar = Grammar::load(
        "IntLit(int) ::= /[0-9]+/
Pair(pair) ::= IntLit[left] IntLit[right]
start ::= Pair

----------- (int)
'Int'

Γ ⊢ left : ?T, Γ ⊢ right : ?T
--------------------------- (pair)
?T",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("1 2", CtxId(0)).unwrap();
    let node = parser.arena().node(state.roots[0]).unwrap();

    assert_eq!(node.status, NodeStatus::Complete);
}

#[test]
fn dsl_supports_direct_left_recursion() {
    let grammar = Grammar::load(
        "Num(num) ::= /[0-9]+/
Expr(add) ::= Expr[left] '+' Num[right]
Expr(base) ::= Num[n]
start ::= Expr

----------- (num)
'Int'

Γ ⊢ n : 'Int'
---------------- (base)
'Int'

Γ ⊢ left : 'Int', Γ ⊢ right : 'Int'
----------------------------------- (add)
'Int'",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("1 + 2", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn dsl_supports_direct_right_recursion() {
    let grammar = Grammar::load(
        "Num(num) ::= /[0-9]+/
Expr(base) ::= Num[n]
Expr(add) ::= Num[left] '+' Expr[right]
start ::= Expr

----------- (num)
'Int'

Γ ⊢ n : 'Int'
---------------- (base)
'Int'

Γ ⊢ left : 'Int', Γ ⊢ right : 'Int'
----------------------------------- (add)
'Int'",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime);
    let state = parser.parse("1 + 2", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn dsl_supports_indirect_recursion() {
    let grammar = Grammar::load(
        "Atom ::= 'x'
A ::= B | Atom
B ::= A
start ::= A",
    )
    .unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping).with_max_depth(16);
    let state = parser.parse("x", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn dsl_supports_binary_recursion_on_both_sides() {
    let grammar = Grammar::load(
        "Num(num) ::= /[0-9]+/
Expr(add) ::= Expr[left] '+' Expr[right]
Expr(base) ::= Num[n]
start ::= Expr

----------- (num)
'Int'

Γ ⊢ n : 'Int'
---------------- (base)
'Int'

Γ ⊢ left : 'Int', Γ ⊢ right : 'Int'
----------------------------------- (add)
'Int'",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(24);
    let state = parser.parse("1 + 2", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn dsl_supports_indirect_binary_expression_layers() {
    let grammar = Grammar::load(
        "Num(num) ::= /[0-9]+/
Add(add) ::= Expr[left] '+' Expr[right]
Expr(base) ::= Num[n]
Expr(lift) ::= Add
start ::= Expr

----------- (num)
'Int'

Γ ⊢ n : 'Int'
---------------- (base)
'Int'

Γ ⊢ left : 'Int', Γ ⊢ right : 'Int'
----------------------------------- (add)
'Int'",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(24);
    let state = parser.parse("1 + 2", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn dsl_supports_float_binary_operator() {
    let grammar = Grammar::load(
        r#"Float(float) ::= /[0-9]+\.[0-9]+/
FloatOp ::= '+.'
Expr(bin_float) ::= Expr[left] FloatOp[op] Expr[right]
Expr(base) ::= Float[n]
start ::= Expr

----------- (float)
'Float'

Γ ⊢ n : 'Float'
------------------ (base)
'Float'

Γ ⊢ left : 'Float', Γ ⊢ right : 'Float'
----------------------------------------- (bin_float)
'Float'"#,
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(24);
    let state = parser.parse("1.0 +. 2.5", CtxId(0)).unwrap();

    assert_eq!(state.roots.len(), 1);
}

#[test]
fn meta_typed_parser_increases_depth_until_success() {
    let spec = "Num(num) ::= /[0-9]+/
Expr(add) ::= Expr[left] '+' Num[right]
Expr(base) ::= Num[n]
start ::= Expr

----------- (num)
'Int'

Γ ⊢ n : 'Int'
---------------- (base)
'Int'

Γ ⊢ left : 'Int', Γ ⊢ right : 'Int'
----------------------------------- (add)
'Int'";
    let grammar = Grammar::load(spec).unwrap();
    let parser = TypedParser::new(grammar.clone(), RuleRuntime::new(grammar));
    let meta = MetaTypedParser::new(parser)
        .with_start_depth(1)
        .with_max_depth(16);

    let (state, depth) = meta.parse("1 + 2", CtxId(0)).unwrap();

    assert!(depth > 1);
    assert_eq!(state.roots.len(), 1);
}

// ============================================================================
// Minimal nested-lambda DSL — isolates context extension + nested bindings
// Reproduces STLC λx:A.λ failure without the full grammar
// ============================================================================

#[test]
fn dsl_nested_lambda_prefix() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    // Minimal grammar: λ a : T . E
    // The inner λ's param binding has no value yet when the outer λ
    // extends context for the body. This mirrors eval.rs:615-624 where
    // a None binding on an incomplete tree returns Partial, not Fail.
    let grammar = Grammar::load(
        "TypeName ::= /[A-Z]+/
        Type ::= TypeName
        Identifier ::= /[a-z]+/
        Var(var) ::= Identifier[x]
        Lam(lam) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expr[e]
        Expr ::= Var | Lam
        start ::= Expr

        x ∈ Γ
        ----------- (var)
        Γ(x)

        Γ[a:τ] ⊢ e : ?R
        ----------------- (lam)
        τ -> ?R",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(48);

    // "λx:A." — outer lambda body is just a dot, inner lambda hasn't started
    let state = parser.parse("λx:A.", CtxId(0)).unwrap();
    assert!(!state.roots.is_empty());

    // "λx:A.λ" — inner lambda param binding exists but has no value yet
    let state = parser.parse("λx:A.λ", CtxId(0)).unwrap();
    assert!(!state.roots.is_empty());

    // "λx:A.λy:B." — nested lambda, both params bound, bodies incomplete
    let state = parser.parse("λx:A.λy:B.", CtxId(0)).unwrap();
    assert!(!state.roots.is_empty());
}

#[test]
fn dsl_nested_lambda_complete() {
    let grammar = Grammar::load(
        "TypeName ::= /[A-Z]+/
        Type ::= TypeName
        Identifier ::= /[a-z]+/
        Var(var) ::= Identifier[x]
        Lam(lam) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expr[e]
        Expr ::= Var | Lam
        start ::= Expr

        x ∈ Γ
        ----------- (var)
        Γ(x)

        Γ[a:τ] ⊢ e : ?R
        ----------------- (lam)
        τ -> ?R",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(48);

    let state = parser.parse("λx:A.x", CtxId(0)).unwrap();
    assert_eq!(state.roots.len(), 1);
    let node = parser.arena().node(state.roots[0]).unwrap();
    assert_eq!(node.status, NodeStatus::Complete);
}

#[test]
fn dsl_triple_nested_lambda() {
    let grammar = Grammar::load(
        "TypeName ::= /[A-Z]+/
        Type ::= TypeName
        Identifier ::= /[a-z]+/
        Var(var) ::= Identifier[x]
        Lam(lam) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expr[e]
        Expr ::= Var | Lam
        start ::= Expr

        x ∈ Γ
        ----------- (var)
        Γ(x)

        Γ[a:τ] ⊢ e : ?R
        ----------------- (lam)
        τ -> ?R",
    )
    .unwrap();
    let runtime = RuleRuntime::new(grammar.clone());
    let mut parser = TypedParser::new(grammar, runtime).with_max_depth(48);

    // Triple nested, all params bound, bodies incomplete
    let state = parser.parse("λx:A.λy:B.λ", CtxId(0)).unwrap();
    assert!(!state.roots.is_empty());

    // Triple nested, complete with var reference
    let state = parser.parse("λx:A.λy:B.λz:C.x", CtxId(0)).unwrap();
    assert_eq!(state.roots.len(), 1);
}
