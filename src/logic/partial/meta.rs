//! MetaParser: wraps Parser, searches for minimum recursion depth.

use std::collections::HashSet;

use crate::debug_info;
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{SppfForest, SppfNodeId};
use crate::logic::partial::{ParseError, Parser, PartialAST, PartialParseOutcome};
use crate::logic::typing::{Context, TypedAST};

const DEFAULT_START_DEPTH: usize = 5;
pub const META_DEFAULT_START_DEPTH: usize = DEFAULT_START_DEPTH;
const DEFAULT_MAX_DEPTH: usize = 256;
const DEFAULT_DEPTH_FACTOR: f64 = 1.5;
const DEFAULT_POST_SUCCESS_STEPS: usize = 2;

pub struct MetaParser {
    parser: Parser,
    start_depth: usize,
    max_depth: usize,
    depth_factor: f64,
    last_used_depth: Option<usize>,
    post_success_steps: usize,
}

impl MetaParser {
    pub fn new(grammar: Grammar) -> Self {
        Self {
            parser: Parser::new(grammar),
            start_depth: DEFAULT_START_DEPTH,
            max_depth: DEFAULT_MAX_DEPTH,
            depth_factor: DEFAULT_DEPTH_FACTOR,
            last_used_depth: None,
            post_success_steps: DEFAULT_POST_SUCCESS_STEPS,
        }
    }

    pub fn from_parser(parser: Parser) -> Self {
        Self {
            parser,
            start_depth: DEFAULT_START_DEPTH,
            max_depth: DEFAULT_MAX_DEPTH,
            depth_factor: DEFAULT_DEPTH_FACTOR,
            last_used_depth: None,
            post_success_steps: DEFAULT_POST_SUCCESS_STEPS,
        }
    }

    pub fn with_start_depth(mut self, depth: usize) -> Self {
        self.start_depth = depth;
        self
    }

    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    pub fn with_depth_factor(mut self, factor: f64) -> Self {
        self.depth_factor = factor.max(1.01);
        self
    }

    pub fn with_post_success_steps(mut self, steps: usize) -> Self {
        self.post_success_steps = steps;
        self
    }

    //parse but discard depth
    pub fn parse(&mut self, input: &str) -> Result<PartialAST, String> {
        let ast = self.partial(input)?;
        let complete_root_ids: Vec<SppfNodeId> = ast
            .root_ids()
            .iter()
            .copied()
            .filter(|root_id| ast.forest().node_is_complete(*root_id))
            .collect();

        if complete_root_ids.is_empty() {
            Err("No complete parse roots found".to_string())
        } else {
            Ok(PartialAST::from_forest(
                ast.forest().clone(),
                complete_root_ids,
                ast.input.clone(),
            ))
        }
    }

    pub fn partial(&mut self, input: &str) -> Result<PartialAST, String> {
        let mut current_depth = self.start_depth;
        debug_info!(
            "meta",
            "MetaParser: Starting partial with initial depth {}",
            current_depth
        );

        let mut best_forest: Option<SppfForest> = None;
        let mut best_roots: HashSet<SppfNodeId> = HashSet::new();
        let mut remaining_post_success_steps: Option<usize> = None;

        while current_depth <= self.max_depth {
            self.parser.set_max_recursion(current_depth);
            debug_info!(
                "meta",
                "MetaParser: Trying depth {} for input '{}'",
                current_depth,
                input
            );
            match self.parser.partial(input) {
                PartialParseOutcome::Success { ast } => {
                    debug_info!("meta", "MetaParser: Success at depth {}", current_depth);
                    self.last_used_depth = Some(current_depth);

                    match best_forest.as_mut() {
                        None => {
                            best_roots.extend(ast.root_ids().iter().copied());
                            best_forest = Some(ast.forest().clone());
                        }
                        Some(forest) => {
                            let id_map = forest.merge_from(ast.forest());
                            for root_id in ast.root_ids() {
                                best_roots.insert(id_map[*root_id]);
                            }
                        }
                    }

                    if remaining_post_success_steps.is_none() && self.parser.last_hit_depth_limit()
                    {
                        remaining_post_success_steps = Some(self.post_success_steps);
                    }

                    if let Some(remaining) = remaining_post_success_steps {
                        if remaining > 0 {
                            let next_depth = current_depth + 1;
                            debug_info!(
                                "meta",
                                "MetaParser: Post-success increment {} -> {} (remaining={})",
                                current_depth,
                                next_depth,
                                remaining
                            );
                            current_depth = next_depth;
                            remaining_post_success_steps = Some(remaining - 1);
                            continue;
                        }
                    }

                    if !best_roots.is_empty() {
                        if let Some(forest) = best_forest {
                            return Ok(PartialAST::from_forest(
                                forest,
                                best_roots.into_iter().collect(),
                                input.to_string(),
                            ));
                        }
                    }
                }
                PartialParseOutcome::Failure(ParseError::DepthLimit) => {
                    if let Some(remaining) = remaining_post_success_steps {
                        if remaining > 0 {
                            let next_depth = current_depth + 1;
                            debug_info!(
                                "meta",
                                "MetaParser: Post-success depth-limit increment {} -> {} (remaining={})",
                                current_depth,
                                next_depth,
                                remaining
                            );
                            current_depth = next_depth;
                            remaining_post_success_steps = Some(remaining - 1);
                            continue;
                        } else if !best_roots.is_empty() {
                            if let Some(forest) = best_forest {
                                return Ok(PartialAST::from_forest(
                                    forest,
                                    best_roots.into_iter().collect(),
                                    input.to_string(),
                                ));
                            }
                        }
                    }

                    let mut next_depth =
                        ((current_depth as f64) * self.depth_factor).ceil() as usize;
                    if next_depth <= current_depth {
                        next_depth = current_depth + 1;
                    }
                    // If the geometric jump would overshoot max_depth, clamp to max_depth
                    // so we always try the exact cap before giving up.
                    if next_depth > self.max_depth && current_depth < self.max_depth {
                        next_depth = self.max_depth;
                    }
                    debug_info!(
                        "meta",
                        "MetaParser: Incrementing at depth {} -> {} (factor={})",
                        current_depth,
                        next_depth,
                        self.depth_factor
                    );
                    current_depth = next_depth;
                }
                PartialParseOutcome::Failure(ParseError::NoValidParse) => {
                    if let Some(remaining) = remaining_post_success_steps {
                        if remaining > 0 {
                            let next_depth = current_depth + 1;
                            debug_info!(
                                "meta",
                                "MetaParser: Post-success no-parse increment {} -> {} (remaining={})",
                                current_depth,
                                next_depth,
                                remaining
                            );
                            current_depth = next_depth;
                            remaining_post_success_steps = Some(remaining - 1);
                            continue;
                        } else if !best_roots.is_empty() {
                            if let Some(forest) = best_forest {
                                return Ok(PartialAST::from_forest(
                                    forest,
                                    best_roots.into_iter().collect(),
                                    input.to_string(),
                                ));
                            }
                        }
                    }

                    debug_info!(
                        "meta",
                        "MetaParser: No valid parse at depth {} - stopping search",
                        current_depth
                    );
                    break;
                }
                PartialParseOutcome::Failure(_) => {
                    if !best_roots.is_empty() {
                        if let Some(forest) = best_forest {
                            return Ok(PartialAST::from_forest(
                                forest,
                                best_roots.into_iter().collect(),
                                input.to_string(),
                            ));
                        }
                    }
                    debug_info!(
                        "meta",
                        "MetaParser: Parse error at depth {} - stopping search",
                        current_depth
                    );
                    break;
                }
            }
        }

        if !best_roots.is_empty() {
            if let Some(forest) = best_forest {
                return Ok(PartialAST::from_forest(
                    forest,
                    best_roots.into_iter().collect(),
                    input.to_string(),
                ));
            }
        }

        Err(format!(
            "No parse results after trying depths {} to {}",
            self.start_depth, self.max_depth
        ))
    }

    pub fn partial_typed(&mut self, input: &str) -> Result<TypedAST, String> {
        let ast = self.partial(input)?;
        ast.typed(&self.parser().grammar)
    }

    pub fn partial_typed_ctx(&mut self, input: &str, ctx: &Context) -> Result<TypedAST, String> {
        let ast = self.partial(input)?;
        let grammar = self.parser().grammar.clone();
        ast.typed_ctx(&grammar, ctx)
    }

    pub fn partial_with_depth(&mut self, input: &str) -> Result<(PartialAST, usize), String> {
        let ast = self.partial(input)?;
        Ok((ast, self.last_used_depth.unwrap_or(self.start_depth)))
    }

    pub fn partial_typed_ctx_with_depth(
        &mut self,
        input: &str,
        ctx: &Context,
    ) -> Result<(TypedAST, usize), String> {
        let ast = self.partial(input)?;
        let typed = ast.typed_ctx(&self.parser().grammar, ctx)?;
        Ok((typed, self.last_used_depth.unwrap_or(self.start_depth)))
    }

    pub fn partial_with_bounds(
        &mut self,
        input: &str,
        start_depth: usize,
        max_depth: usize,
    ) -> Result<(PartialAST, usize), String> {
        let prev_start = self.start_depth;
        let prev_max = self.max_depth;

        self.start_depth = start_depth;
        self.max_depth = max_depth.max(start_depth);

        let result = self.partial_with_depth(input);
        self.start_depth = prev_start;
        self.max_depth = prev_max;

        result
    }

    pub fn clear_cache(&mut self) {
        self.parser.clear_cache();
        self.last_used_depth = None;
    }

    /// Returns the depth at which the last successful parse settled.
    pub fn last_used_depth(&self) -> Option<usize> {
        self.last_used_depth
    }

    pub fn parser(&self) -> &Parser {
        &self.parser
    }

    pub fn parser_mut(&mut self) -> &mut Parser {
        &mut self.parser
    }
}
