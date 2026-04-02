//! Incremental parsing — extend existing roots with new tokens.
//!
//! # Invariant
//! `advance` can only **reduce** or **extend** roots. New roots are never
//! created — every surviving root must be an extension of a previous partial
//! parse. This is enforced by only parsing from the frontier of existing roots.
//!
//! # Algorithm
//! 1. For each previous root, try to extend it with new tokens.
//! 2. Roots that can't accept the new token are dead (filtered out).
//! 3. Extended roots that become complete are kept.
//! 4. Extended roots that remain partial at the new input boundary are kept.
//!
//! # Complexity
//! Time: O(new_tokens × root_count × frontier_depth)
//! Space: O(new_nodes) — arena is appended to, never cleared.

use crate::logic::grammar::{Production, Segment, Symbol};
use crate::logic::parse::arena::{
    ArenaNode, ChildRef, CtxId, NodeId, NodeStatus, NtId, PackedAlt, PathId, ProdId, Span,
    TokenRef, TypeId, TypeStatus,
};
use crate::logic::parse::parser::{Branch, TypedParser};
use crate::regex::PrefixStatus;

use crate::logic::fusion::{
    DepthMeta, TransitionError, TypedPrefixError, TypedPrefixState, TypingRuntime, TypingState,
};
use crate::logic::typing::state::BindingValue;

/// Incremental advance: reuse arena, only extend existing roots.
pub fn incremental<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    prev: &TypedPrefixState,
    input: &str,
    ctx: CtxId,
) -> Result<TypedPrefixState, TypedPrefixError> {
    let segments = parser
        .grammar()
        .tokenize(input)
        .map_err(|err| TypedPrefixError::rejected(input.len(), 0, err))?;

    let new_len = segments.len();

    // For each existing root, try to extend it with new tokens.
    // Roots that can't be extended are dead.
    let mut new_roots = Vec::new();
    for &root_id in &prev.roots {
        let Some(root_ref) = parser.arena().node(root_id) else {
            continue;
        };
        let root = root_ref.clone();
        drop(root_ref);

        // If root is complete and already covers the full new input, keep it.
        if root.status == NodeStatus::Complete && root.span.end as usize >= new_len {
            new_roots.push(root_id);
            continue;
        }

        // Try to extend any surviving root. A root-level status of `Complete`
        // is only a summary over packed alternatives; some alts may still be
        // partial and therefore extendable.
        new_roots.extend(extend_root(parser, root_id, &root, &segments, ctx)?);
        // If extend_root returns no nodes, the root is dead — don't add it.
    }

    // Filter: only keep roots that are partial at the end or complete covering full input.
    new_roots.retain(|&node_id| {
        parser.arena().node(node_id).is_some_and(|node| {
            let covers_end = node.span.end as usize == new_len;
            covers_end
                && (node.status == NodeStatus::Partial || node.status == NodeStatus::Complete)
        })
    });

    if new_roots.is_empty() {
        return Err(TypedPrefixError::rejected(
            input.len(),
            parser.arena().node_count() as u16,
            "no typed branches survived",
        ));
    }

    Ok(TypedPrefixState {
        input_len: input.len(),
        roots: new_roots,
        frontier: Vec::new(),
        depth: DepthMeta {
            searched_depth: 0,
            hit_depth_limit: false,
            depth_failures: 0,
        },
    })
}

/// Try to extend a partial root with new tokens.
/// Returns all extended root node ids that remain viable.
fn extend_root<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    root_id: NodeId,
    root: &ArenaNode,
    segments: &[Segment],
    ctx: CtxId,
) -> Result<Vec<NodeId>, TypedPrefixError> {
    let root_end = root.span.end as usize;
    let new_end = segments.len();

    // No new tokens to add
    if root_end >= new_end {
        return Ok(Vec::new());
    }

    let mut extended_ids = Vec::new();
    let Some(alts) = parser
        .arena()
        .alts_for(root_id)
        .map(|alts| alts.iter().cloned().collect::<Vec<_>>())
    else {
        return Ok(extended_ids);
    };

    for alt in alts {
        let prod = alt.prod;
        let Some(production) = parser.prod(prod) else {
            continue;
        };
        let rhs = production.rhs.clone();
        let alt_idx = parser.alt(prod) as u16;
        let mut alt_root_end = root_end;

        // How many children we already have.
        //
        // Important: in a partial parse, the last child is often a *placeholder*
        // terminal with `complete=false` (created when input was exhausted).
        // When advancing with more tokens, we need to *re-parse that symbol* and
        // replace the placeholder, rather than treating it as a fully consumed slot.
        let mut child_count = alt.children.len();
        if let Some(ChildRef::Terminal(tok)) = alt.children.last()
            && !tok.complete
        {
            // Drop the placeholder child; resume parsing at its start index.
            alt_root_end = tok.start as usize;
            child_count = child_count.saturating_sub(1);

            // If the symbol immediately before the placeholder is a zero-width
            // node, it may have matched via an epsilon branch and still be
            // extensible now that more input is available. Reparse from that
            // child instead of forcing progress only into the trailing symbol.
            if let Some(ChildRef::Node(prev_id)) = child_count
                .checked_sub(1)
                .and_then(|idx| alt.children.get(idx))
                && let Some(prev) = parser.arena().node(*prev_id)
                && prev.span.start == prev.span.end
            {
                alt_root_end = prev.span.start as usize;
                child_count = child_count.saturating_sub(1);
            }
        }

        if let Some(ChildRef::Node(last_id)) = child_count
            .checked_sub(1)
            .and_then(|idx| alt.children.get(idx))
            && let Some(last_node) = parser.arena().node(*last_id)
            && last_node.status == NodeStatus::Complete
            && last_node.span.start == last_node.span.end
        {
            alt_root_end = last_node.span.start as usize;
            child_count = child_count.saturating_sub(1);
        }

        let next_sym_idx = child_count;
        if next_sym_idx >= rhs.len() {
            let Some(last_child) = alt.children.last() else {
                continue;
            };
            let tail_idx = alt.children.len().saturating_sub(1);
            let tail_start = match last_child {
                ChildRef::Terminal(tok) => tok.start as usize,
                ChildRef::Node(child_id) => parser
                    .arena()
                    .node(*child_id)
                    .map(|node| node.span.start as usize)
                    .unwrap_or(root_end),
            };
            let Some((base, prefix)) =
                rebuild_prefix_states(parser, root, &alt, &rhs, segments, tail_idx, alt_idx)?
            else {
                continue;
            };

            let branches = suffix(
                parser,
                &rhs[tail_idx..],
                segments,
                tail_start,
                0,
                base.clone(),
                prod,
                prefix.clone(),
                None,
                alt_idx,
                tail_idx as u16,
            )?;

            for branch in branches {
                let new_children: Vec<ChildRef> = alt.children[..tail_idx]
                    .iter()
                    .cloned()
                    .chain(branch.children)
                    .collect();
                let mut child_states = Vec::with_capacity(prefix.len() + branch.states.len());
                child_states.extend(prefix.iter().cloned());
                child_states.extend(branch.states);
                if let Some(id) = finish_alt(
                    parser,
                    root,
                    prod,
                    new_children,
                    child_states,
                    ctx,
                    tail_start as u32,
                )? {
                    extended_ids.push(id);
                }
            }
            continue;
        }

        let Some((base, prefix)) =
            rebuild_prefix_states(parser, root, &alt, &rhs, segments, child_count, alt_idx)?
        else {
            continue;
        };

        let branches = suffix(
            parser,
            &rhs[next_sym_idx..],
            segments,
            alt_root_end,
            0,
            base.clone(),
            prod,
            prefix,
            None,
            alt_idx,
            next_sym_idx as u16,
        )?;

        for branch in branches {
            let new_children: Vec<ChildRef> = alt.children[..child_count]
                .iter()
                .cloned()
                .chain(branch.children)
                .collect();
            if let Some(id) = finish_alt(
                parser,
                root,
                prod,
                new_children,
                branch.states,
                ctx,
                root.span.start,
            )? {
                extended_ids.push(id);
            }
        }
    }

    // Left-corner growth is only needed when direct frontier extension does not
    // already produce viable continuations.
    if extended_ids.is_empty() {
        extended_ids.extend(extend_via_left_corner_component(
            parser, root_id, root, segments, ctx,
        )?);
    }
    extended_ids.sort_by_key(|id| id.0);
    extended_ids.dedup_by_key(|id| id.0);

    Ok(extended_ids)
}

fn extend_via_left_corner_component<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    seed_id: NodeId,
    seed: &ArenaNode,
    segments: &[Segment],
    ctx: CtxId,
) -> Result<Vec<NodeId>, TypedPrefixError> {
    let target = seed.nt;
    let component = parser.left_component(target);
    if component.len() <= 1 && !parser.has_self_left_recursion(target) {
        return Ok(Vec::new());
    }

    let component_set = component
        .iter()
        .map(|nt| nt.0)
        .collect::<std::collections::HashSet<_>>();
    let mut results = component
        .iter()
        .copied()
        .map(|nt| (nt, Vec::<NodeId>::new()))
        .collect::<std::collections::HashMap<_, _>>();
    let mut seen = component
        .iter()
        .copied()
        .map(|nt| (nt, std::collections::HashSet::<(u32, u32, usize)>::new()))
        .collect::<std::collections::HashMap<_, _>>();

    results.entry(target).or_default().push(seed_id);
    seen.entry(target)
        .or_default()
        .insert((seed.span.start, seed.span.end, usize::MAX));

    let state = TypingState {
        ctx,
        expected: None,
        inferred: None,
        path: None,
        bindings: Vec::new(),
    };

    let mut changed = true;
    while changed {
        changed = false;
        for &nt in &component {
            let Some(productions) = parser.grammar().productions_by_idx(nt.0).cloned() else {
                continue;
            };
            for (alt_idx, production) in productions.iter().enumerate() {
                let Some(Symbol::Nonterminal { name, .. }) = production.rhs.first() else {
                    continue;
                };
                let Some(first_idx) = parser.grammar().nt_index(name) else {
                    continue;
                };
                if !component_set.contains(&first_idx) {
                    continue;
                }
                let seeds = results.get(&NtId(first_idx)).cloned().unwrap_or_default();
                for seed in seeds {
                    if let Some(node_id) =
                        grow(parser, nt, alt_idx, production, seed, segments, 1, &state)?
                        && let Some(node) = parser.arena().node(node_id)
                    {
                        let sig = (node.span.start, node.span.end, alt_idx);
                        if seen.get_mut(&nt).unwrap().insert(sig) {
                            results.get_mut(&nt).unwrap().push(node_id);
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    Ok(results
        .remove(&target)
        .unwrap_or_default()
        .into_iter()
        .filter(|id| *id != seed_id)
        .collect())
}

// Rebuild one parent alternative after its frontier has advanced.
fn finish_alt<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    root: &ArenaNode,
    prod: ProdId,
    children: Vec<ChildRef>,
    child_states: Vec<TypingState>,
    ctx: CtxId,
    span_start: u32,
) -> Result<Option<NodeId>, TypedPrefixError> {
    let base = TypingState {
        ctx,
        expected: None,
        inferred: None,
        path: None,
        bindings: Vec::new(),
    };
    let rhs_len = parser
        .prod(prod)
        .map(|production| production.rhs.len())
        .unwrap_or(children.len());
    let status = if children.len() == rhs_len && children.iter().all(|child| match child {
        ChildRef::Terminal(tok) => tok.complete,
        ChildRef::Node(node_id) => parser
            .arena()
            .node(*node_id)
            .is_some_and(|node| node.status == NodeStatus::Complete),
    }) {
        NodeStatus::Complete
    } else {
        NodeStatus::Partial
    };

    match parser_typing(parser).finish_production(prod, &base, &child_states, status) {
        Ok(out) => {
            let inferred = match out.inferred {
                Some(inferred) => inferred,
                None if matches!(status, NodeStatus::Partial) => TypeId(0),
                None => return Ok(None),
            };
            let ty = match status {
                NodeStatus::Complete => TypeStatus::Valid(inferred),
                NodeStatus::Partial => TypeStatus::Partial(inferred),
            };
            let span_end = children
                .iter()
                .filter_map(|c| match c {
                    ChildRef::Terminal(tok) => Some(tok.end),
                    ChildRef::Node(id) => parser.arena().node(*id).map(|n| n.span.end),
                })
                .max()
                .unwrap_or(root.span.end);
            Ok(Some(parser.arena().push_node(
                ArenaNode {
                    nt: root.nt,
                    span: Span {
                        start: span_start,
                        end: span_end,
                    },
                    status,
                    ty,
                    env_in: ctx,
                    env_out: out.ctx,
                    bindings: out.bindings,
                    alts: crate::logic::parse::arena::AltRange { start: 0, len: 0 },
                },
                vec![PackedAlt { prod, children }],
            )))
        }
        Err(TransitionError::TooDeep | TransitionError::Rejected) => Ok(None),
    }
}

// Replay the already-parsed prefix of an alternative so the next suffix parse
// starts from the same typed state as the original branch.
fn rebuild_prefix_states<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    root: &ArenaNode,
    alt: &PackedAlt,
    rhs: &[Symbol],
    segments: &[Segment],
    child_count: usize,
    alt_idx: u16,
) -> Result<Option<(TypingState, Vec<TypingState>)>, TypedPrefixError> {
    let mut state = TypingState {
        ctx: root.env_in,
        expected: None,
        inferred: None,
        path: None,
        bindings: Vec::new(),
    };
    let mut prefix = Vec::with_capacity(child_count);

    for child_idx in 0..child_count {
        let Some(symbol) = rhs.get(child_idx) else {
            return Ok(None);
        };
        let prep = match parser_typing(parser).prepare_child(
            alt.prod,
            child_idx,
            symbol.binding().map(String::as_str),
            &state,
            &prefix,
        ) {
            Ok(state) => state,
            Err(TransitionError::Rejected | TransitionError::TooDeep) => return Ok(None),
        };
        let path = parser
            .arena_mut()
            .push_path(state.path, child_idx as u16, alt_idx);
        let descended = match parser_typing(parser).descend(
            &prep,
            path,
            symbol.binding().map(String::as_str),
        ) {
            Ok(state) => state,
            Err(TransitionError::Rejected | TransitionError::TooDeep) => return Ok(None),
        };

        let Some(child) = alt.children.get(child_idx) else {
            return Ok(None);
        };

        state = match (symbol, child) {
            (Symbol::Terminal { regex, .. }, ChildRef::Terminal(tok)) => {
                let segment = if tok.complete {
                    segments.get(tok.start as usize)
                } else {
                    None
                };
                match parser_typing(parser).finish_terminal_child(&descended, path, regex, segment)
                {
                    Ok(next) => next,
                    Err(TransitionError::Rejected | TransitionError::TooDeep) => return Ok(None),
                }
            }
            (Symbol::Nonterminal { .. }, ChildRef::Node(node_id)) => {
                let Some(node) = parser.arena().node(*node_id) else {
                    return Ok(None);
                };
                parser_typing(parser).finish_node_child(
                    &descended,
                    path,
                    &node,
                    &node.bindings,
                    segments,
                )
            }
            _ => return Ok(None),
        };

        prefix.push(state.clone());
    }

    Ok(Some((state, prefix)))
}

fn parser_typing<T: TypingRuntime>(parser: &TypedParser<T>) -> &T {
    parser.typing()
}

#[allow(clippy::too_many_arguments)]
// Step through a suffix under incremental advance. This mirrors parser-side
// symbol stepping but stays local to the incremental frontier.
fn suffix<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    symbols: &[Symbol],
    segments: &[Segment],
    input_idx: usize,
    depth: u16,
    state: TypingState,
    prod: ProdId,
    parsed_prefix: Vec<TypingState>,
    parent_path: Option<PathId>,
    alt_idx: u16,
    child_idx: u16,
) -> Result<Vec<Branch>, TypedPrefixError> {
    // We need access to the parser's private parse_symbols method.
    // Instead of making it public, we replicate the logic here since
    // it's the hot path for incremental parsing.
    parse_symbols(
        parser,
        symbols,
        segments,
        input_idx,
        depth,
        state,
        prod,
        parsed_prefix,
        parent_path,
        alt_idx,
        child_idx,
    )
}

// ============================================================================
// Parse a suffix under an already-established typed prefix.
// ============================================================================

#[allow(clippy::too_many_arguments)]
fn parse_symbols<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    symbols: &[Symbol],
    segments: &[Segment],
    input_idx: usize,
    depth: u16,
    state: TypingState,
    prod: ProdId,
    parsed_prefix: Vec<TypingState>,
    parent_path: Option<PathId>,
    alt_idx: u16,
    child_idx: u16,
) -> Result<Vec<Branch>, TypedPrefixError> {
    if symbols.is_empty() {
        return Ok(vec![Branch {
            children: Vec::new(),
            states: Vec::new(),
            end: input_idx,
            status: NodeStatus::Complete,
        }]);
    }

    let first = &symbols[0];
    let rest = &symbols[1..];
    let state = match parser_typing(parser).prepare_child(
        prod,
        child_idx as usize,
        first.binding().map(String::as_str),
        &state,
        &parsed_prefix,
    ) {
        Ok(state) => state,
        Err(TransitionError::Rejected) => return Ok(Vec::new()),
        Err(TransitionError::TooDeep) => return Ok(Vec::new()),
    };

    let first_branches = parse_symbol(
        parser,
        first,
        segments,
        input_idx,
        depth,
        state,
        parent_path,
        alt_idx,
        child_idx,
    )?;
    let mut out = Vec::new();

    for first_branch in first_branches {
        if first_branch.status == NodeStatus::Partial || rest.is_empty() {
            out.push(first_branch);
            continue;
        }
        let Some(next_state) = first_branch.states.last().cloned() else {
            continue;
        };
        let mut next_prefix = Vec::with_capacity(parsed_prefix.len() + first_branch.states.len());
        next_prefix.extend(parsed_prefix.iter().cloned());
        next_prefix.extend(first_branch.states.iter().cloned());
        let rest_branches = parse_symbols(
            parser,
            rest,
            segments,
            first_branch.end,
            depth,
            next_state,
            prod,
            next_prefix,
            parent_path,
            alt_idx,
            child_idx + 1,
        )?;
        for rest_branch in rest_branches {
            let mut children =
                Vec::with_capacity(first_branch.children.len() + rest_branch.children.len());
            children.extend(first_branch.children.iter().cloned());
            children.extend(rest_branch.children);
            let mut states =
                Vec::with_capacity(first_branch.states.len() + rest_branch.states.len());
            states.extend(first_branch.states.iter().cloned());
            states.extend(rest_branch.states);
            let status = if first_branch.status == NodeStatus::Partial
                || rest_branch.status == NodeStatus::Partial
            {
                NodeStatus::Partial
            } else {
                NodeStatus::Complete
            };
            out.push(Branch {
                children,
                states,
                end: rest_branch.end,
                status,
            });
        }
    }

    Ok(out)
}

#[allow(clippy::too_many_arguments)]
fn parse_symbol<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    symbol: &Symbol,
    segments: &[Segment],
    input_idx: usize,
    depth: u16,
    state: TypingState,
    parent_path: Option<PathId>,
    alt_idx: u16,
    child_idx: u16,
) -> Result<Vec<Branch>, TypedPrefixError> {
    let path = parser
        .arena_mut()
        .push_path(parent_path, child_idx, alt_idx);
    let state =
        match parser_typing(parser).descend(&state, path, symbol.binding().map(String::as_str)) {
            Ok(state) => state,
            Err(TransitionError::Rejected) => return Ok(Vec::new()),
            Err(TransitionError::TooDeep) => return Ok(Vec::new()),
        };

    match symbol {
        Symbol::Terminal { regex, .. } => parse_terminal(parser, regex, segments, input_idx, state),
        Symbol::Nonterminal { name, .. } => {
            let Some(nt_idx) = parser.grammar().nt_index(name) else {
                return Ok(Vec::new());
            };
            let nodes = nt(
                parser,
                NtId(nt_idx),
                segments,
                input_idx,
                depth,
                state.clone(),
            )?;
            let mut out = Vec::new();
            for node_id in nodes {
                let Some(node) = parser.arena().node(node_id) else {
                    continue;
                };
                out.push(Branch {
                    children: vec![ChildRef::Node(node_id)],
                    states: vec![parser_typing(parser).finish_node_child(
                        &state,
                        path,
                        &node,
                        &node.bindings,
                        segments,
                    )],
                    end: node.span.end as usize,
                    status: node.status,
                });
            }
            Ok(out)
        }
    }
}

fn parse_terminal<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    regex: &crate::regex::Regex,
    segments: &[Segment],
    input_idx: usize,
    state: TypingState,
) -> Result<Vec<Branch>, TypedPrefixError> {
    let segment = segments.get(input_idx);
    let next = parser_typing(parser).finish_terminal_child(
        &state,
        state.path.unwrap_or(PathId(0)),
        regex,
        segment,
    );
    let Ok(mut next) = next else {
        return Ok(Vec::new());
    };
    if next.inferred.is_none() {
        next.inferred = Some(TypeId(0));
    }

    let (end, status, child) = match segment {
        Some(segment) => match regex.prefix_match(segment.as_str()) {
            PrefixStatus::NoMatch => return Ok(Vec::new()),
            PrefixStatus::Complete | PrefixStatus::Extensible(_) => (
                input_idx + 1,
                NodeStatus::Complete,
                ChildRef::Terminal(TokenRef {
                    start: segment.index as u32,
                    end: segment.index as u32 + 1,
                    complete: true,
                }),
            ),
            // Keep incremental terminal behavior consistent with full parse:
            // segments are atomic tokens, so prefix-only regex matches must
            // not consume the segment.
            PrefixStatus::Prefix(_) => return Ok(Vec::new()),
        },
        None => (
            input_idx,
            NodeStatus::Partial,
            ChildRef::Terminal(TokenRef {
                start: input_idx as u32,
                end: input_idx as u32,
                complete: false,
            }),
        ),
    };

    Ok(vec![Branch {
        children: vec![child],
        states: vec![next],
        end,
        status,
    }])
}

// Parse one nonterminal from the incremental frontier.
fn nt<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    nt: NtId,
    segments: &[Segment],
    input_idx: usize,
    depth: u16,
    state: TypingState,
) -> Result<Vec<NodeId>, TypedPrefixError> {
    // Delegate to the parser's public parse_nonterminal via a helper.
    // Since we can't access private methods, we use the parser's
    // existing parse machinery for the new tokens only.
    //
    // For incremental parsing, we only need to parse from input_idx
    // (which is the end of the existing root), so we can use a
    // lightweight approach.

    let component = parser.left_component(nt);
    if component.len() > 1 || parser.has_self_left_recursion(nt) {
        return scc(parser, &component, nt, segments, input_idx, depth, state);
    }

    let Some(productions) = parser.grammar().productions_by_idx(nt.0).cloned() else {
        return Ok(Vec::new());
    };

    let mut nodes = Vec::new();
    let mut seen = std::collections::HashSet::new();
    let nt_name = parser.grammar().nt_name(nt.0).unwrap_or("").to_string();
    let mut recursive = Vec::new();
    for (alt_idx, production) in productions.iter().enumerate() {
        if matches!(
            production.rhs.first(),
            Some(Symbol::Nonterminal { name, .. }) if name == &nt_name
        ) {
            recursive.push((alt_idx, production.clone()));
            continue;
        }
        if let Some(node_id) = prod(
            parser,
            nt,
            alt_idx,
            production,
            segments,
            input_idx,
            depth + 1,
            &state,
        )? {
            if let Some(node) = parser.arena().node(node_id) {
                seen.insert((node.span.start, node.span.end, alt_idx));
            }
            nodes.push(node_id)
        }
    }

    let mut changed = true;
    while changed && !recursive.is_empty() {
        changed = false;
        let seeds = nodes.clone();
        for (alt_idx, production) in &recursive {
            for seed in &seeds {
                if let Some(node_id) = grow(
                    parser,
                    nt,
                    *alt_idx,
                    production,
                    *seed,
                    segments,
                    depth + 1,
                    &state,
                )? {
                    let Some(node) = parser.arena().node(node_id) else {
                        continue;
                    };
                    let sig = (node.span.start, node.span.end, *alt_idx);
                    if !seen.contains(&sig) {
                        seen.insert(sig);
                        nodes.push(node_id);
                        changed = true;
                    }
                }
            }
        }
    }

    Ok(nodes)
}

#[allow(clippy::too_many_arguments)]
// Parse one production and finish it into a node.
fn prod<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    nt: NtId,
    alt_idx: usize,
    production: &Production,
    segments: &[Segment],
    input_idx: usize,
    depth: u16,
    state: &TypingState,
) -> Result<Option<NodeId>, TypedPrefixError> {
    let branches = parse_symbols(
        parser,
        &production.rhs,
        segments,
        input_idx,
        depth,
        state.clone(),
        prod_id(parser, nt, alt_idx),
        Vec::new(),
        state.path,
        alt_idx as u16,
        0,
    )?;
    finish_branches(parser, nt, alt_idx, state, input_idx, branches)
}

// Parse one left-recursive strongly connected component.
fn scc<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    component: &[NtId],
    target: NtId,
    segments: &[Segment],
    input_idx: usize,
    depth: u16,
    state: TypingState,
) -> Result<Vec<NodeId>, TypedPrefixError> {
    let component_set = component
        .iter()
        .map(|nt| nt.0)
        .collect::<std::collections::HashSet<_>>();
    let mut results = component
        .iter()
        .copied()
        .map(|nt| (nt, Vec::<NodeId>::new()))
        .collect::<std::collections::HashMap<_, _>>();
    let mut seen = component
        .iter()
        .copied()
        .map(|nt| (nt, std::collections::HashSet::<(u32, u32, usize)>::new()))
        .collect::<std::collections::HashMap<_, _>>();

    for &nt in component {
        let Some(productions) = parser.grammar().productions_by_idx(nt.0).cloned() else {
            continue;
        };
        for (alt_idx, production) in productions.iter().enumerate() {
            let recursive = matches!(
                production.rhs.first(),
                Some(Symbol::Nonterminal { name, .. })
                    if parser.grammar().nt_index(name).is_some_and(|idx| component_set.contains(&idx))
            );
            if recursive {
                continue;
            }
            if let Some(node_id) = prod(
                parser,
                nt,
                alt_idx,
                production,
                segments,
                input_idx,
                depth + 1,
                &state,
            )? && let Some(node) = parser.arena().node(node_id)
            {
                let sig = (node.span.start, node.span.end, alt_idx);
                if seen.get_mut(&nt).unwrap().insert(sig) {
                    results.get_mut(&nt).unwrap().push(node_id);
                }
            }
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for &nt in component {
            let Some(productions) = parser.grammar().productions_by_idx(nt.0).cloned() else {
                continue;
            };
            for (alt_idx, production) in productions.iter().enumerate() {
                let Some(Symbol::Nonterminal { name, .. }) = production.rhs.first() else {
                    continue;
                };
                let Some(first_idx) = parser.grammar().nt_index(name) else {
                    continue;
                };
                if !component_set.contains(&first_idx) {
                    continue;
                }
                let seeds = results.get(&NtId(first_idx)).cloned().unwrap_or_default();
                for seed in seeds {
                    if let Some(node_id) = grow(
                        parser,
                        nt,
                        alt_idx,
                        production,
                        seed,
                        segments,
                        depth + 1,
                        &state,
                    )? && let Some(node) = parser.arena().node(node_id)
                    {
                        let sig = (node.span.start, node.span.end, alt_idx);
                        if seen.get_mut(&nt).unwrap().insert(sig) {
                            results.get_mut(&nt).unwrap().push(node_id);
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    Ok(results.remove(&target).unwrap_or_default())
}

#[allow(clippy::too_many_arguments)]
// Grow a larger recursive parent from an existing seed.
fn grow<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    nt: NtId,
    alt_idx: usize,
    production: &Production,
    seed: NodeId,
    segments: &[Segment],
    depth: u16,
    state: &TypingState,
) -> Result<Option<NodeId>, TypedPrefixError> {
    let Some(seed) = parser.arena().node(seed).map(|n| n.clone()) else {
        return Ok(None);
    };
    let Some(first) = production.rhs.first() else {
        return Ok(None);
    };
    let path = parser.arena().push_path(state.path, 0, alt_idx as u16);
    let prep = match parser_typing(parser).prepare_child(
        prod_id(parser, nt, alt_idx),
        0,
        first.binding().map(String::as_str),
        state,
        &[],
    ) {
        Ok(state) => state,
        Err(TransitionError::Rejected) => return Ok(None),
        Err(TransitionError::TooDeep) => return Ok(None),
    };
    let descended =
        match parser_typing(parser).descend(&prep, path, first.binding().map(String::as_str)) {
            Ok(state) => state,
            Err(TransitionError::Rejected) => return Ok(None),
            Err(TransitionError::TooDeep) => return Ok(None),
        };
    let first_state =
        parser_typing(parser).finish_node_child(&descended, path, &seed, &seed.bindings, segments);
    let branches = parse_symbols(
        parser,
        &production.rhs[1..],
        segments,
        seed.span.end as usize,
        depth,
        first_state.clone(),
        prod_id(parser, nt, alt_idx),
        vec![first_state],
        state.path,
        alt_idx as u16,
        1,
    )?;
    finish_branches(
        parser,
        nt,
        alt_idx,
        state,
        seed.span.start as usize,
        branches,
    )
}

fn finish_branches<T: TypingRuntime>(
    parser: &mut TypedParser<T>,
    nt: NtId,
    alt_idx: usize,
    state: &TypingState,
    input_idx: usize,
    branches: Vec<Branch>,
) -> Result<Option<NodeId>, TypedPrefixError> {
    let mut packed = Vec::new();
    let mut chosen: Option<(u32, bool, NodeStatus, TypeStatus, CtxId, Vec<BindingValue>)> = None;
    let mut span_end = input_idx as u32;

    for branch in branches {
        match parser_typing(parser).finish_production(
            prod_id(parser, nt, alt_idx),
            state,
            &branch.states,
            branch.status,
        ) {
            Ok(out) => {
                let inferred = match out.inferred {
                    Some(inferred) => inferred,
                    None if matches!(branch.status, NodeStatus::Partial) => TypeId(0),
                    None => continue,
                };
                let status = branch.status;
                let end = branch.end as u32;
                span_end = span_end.max(end);
                if chosen.as_ref().is_none_or(|(best_end, best_complete, ..)| {
                    end > *best_end
                        || (end == *best_end
                            && matches!(status, NodeStatus::Complete)
                            && !best_complete)
                }) {
                    chosen = Some((
                        end,
                        matches!(status, NodeStatus::Complete),
                        status,
                        match status {
                            NodeStatus::Complete => TypeStatus::Valid(inferred),
                            NodeStatus::Partial => TypeStatus::Partial(inferred),
                        },
                        out.ctx,
                        out.bindings.clone(),
                    ));
                }
                packed.push(PackedAlt {
                    prod: prod_id(parser, nt, alt_idx),
                    children: branch.children,
                });
            }
            Err(TransitionError::TooDeep) => {}
            Err(TransitionError::Rejected) => {}
        }
    }

    let Some((_, _, status, ty, env_out, node_bindings)) = chosen else {
        return Ok(None);
    };

    Ok(Some(parser.arena().push_node(
        ArenaNode {
            nt,
            span: Span {
                start: input_idx as u32,
                end: span_end,
            },
            status,
            ty,
            env_in: state.ctx,
            env_out,
            bindings: node_bindings,
            alts: crate::logic::parse::arena::AltRange { start: 0, len: 0 },
        },
        packed,
    )))
}

fn prod_id<T: TypingRuntime>(parser: &TypedParser<T>, nt: NtId, alt: usize) -> ProdId {
    // Compute production id from nt index and alt
    let mut offset = 0usize;
    for idx in 0..parser.grammar().production_count() {
        let len = parser
            .grammar()
            .productions_by_idx(idx)
            .map(|ps| ps.len())
            .unwrap_or(0);
        if idx == nt.0 {
            return ProdId(offset + alt);
        }
        offset += len;
    }
    ProdId(0)
}
