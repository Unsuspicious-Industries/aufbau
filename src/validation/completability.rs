// Completability Validation Tests
//
// Implements the formal framework for completability as defined in docs/challenges.md.
//
// A string s is completable in L if there exists s' such that ss' in L.
// We use a partial parser and typing core to check for completability.
//
// The algorithm explores the partial parse forest and uses the typing core
// to filter out invalid branches based on defined type rules.

use crate::debug_debug;
use crate::logic::ast::ASTNode;
use crate::logic::grammar::Grammar;
use crate::logic::partial::binding::resolve_binding_path;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::structure::{Node, Terminal};
use crate::logic::typing::Context;
use crate::logic::typing::rule::TypingJudgment;
use crate::regex::Regex as DerivativeRegex;
use std::collections::{HashSet, VecDeque};

/// Represents a search state in the completion exploration
#[derive(Clone, Debug)]
struct CompletionState {
    /// Current input string being explored
    input: String,
    /// Depth of exploration (number of tokens added)
    depth: usize,
    /// Path taken to reach this state (for debugging)
    path: Vec<DerivativeRegex>,
}

/// Result of completion exploration
#[derive(Debug)]
pub enum CompletionResult {
    /// Successfully found a complete AST
    Success {
        /// The complete input string that parses to a full AST
        complete_input: String,
        /// The resulting AST
        ast: ASTNode,
        /// The sequence of completion tokens used
        completion_path: Vec<DerivativeRegex>,
        /// Depth required to reach completion
        depth: usize,
    },
    /// No completion found within the search bounds
    Failure {
        /// Maximum depth explored
        max_depth_reached: usize,
        /// Number of unique states explored
        states_explored: usize,
        /// Sample of states that were explored but didn't lead to completion
        visited_states: Vec<String>,
    },

    // the input is invalid, not even partial
    Invalid(String),
    Inconsistency(String),
    /// Error during exploration
    Error(String),
}

/// Complete with empty context
pub fn complete_ast(grammar: &Grammar, input: &str, max_depth: usize) -> CompletionResult {
    complete_ast_with_context(grammar, input, max_depth, Context::new())
}

/// Complete with typing context - always enforces type rules
pub fn complete_ast_with_context(grammar: &Grammar, input: &str, max_depth: usize, ctx: Context) -> CompletionResult {
    // Initialize BFS queue with the starting state
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    let mut states_explored = 0;

    queue.push_back(CompletionState {
        input: input.to_string(),
        depth: 0,
        path: Vec::new(),
    });

    // fist do a partial parse check. If it parses partially its completeable.
    // if it fails to parse partially, its invalid input.
    let mut parser = Parser::new(grammar.clone());
    match parser.partial(input) {
        Ok(_) => { /* valid partial input, continue */ }
        Err(e) => {
            return CompletionResult::Invalid(format!("Input is not even partially valid: {}", e));
        }
    };

    let initial_vars: Vec<String> = ctx.bindings.keys().cloned().collect();

    while let Some(current_state) = queue.pop_front() {
        states_explored += 1;

        // Avoid revisiting the same input string
        if visited.contains(&current_state.input) {
            continue;
        }
        visited.insert(current_state.input.clone());

        // Try to parse the current input
        let mut parser = Parser::new(grammar.clone());
        let partial_ast = match parser.partial(&current_state.input) {
            Ok(ast) => ast,
            Err(e) => return CompletionResult::Invalid(e),
        };

        // Check if we have a complete AND well-typed AST
        if partial_ast.typed_complete_ctx(grammar, &ctx).is_ok() {
            if let Ok(complete_ast) = partial_ast.clone().into_complete() {
                return CompletionResult::Success {
                    complete_input: current_state.input.clone(),
                    ast: complete_ast,
                    completion_path: current_state.path.clone(),
                    depth: current_state.depth,
                };
            }
        }

        // Get completions per root and filter
        let mut all_raw_completions = Vec::new();
        let mut valid_tokens = Vec::new();

        for root in partial_ast.roots() {
            let root_completions = root.collect_valid_tokens(grammar);
            if !root_completions.is_empty() {
                all_raw_completions.extend(root_completions.clone());
                let filtered = type_filtering(grammar, root, &root_completions, &initial_vars);
                valid_tokens.extend(filtered);
            }
        }

        if all_raw_completions.is_empty() {
            // CRITICAL BUG: We followed completion engine suggestions but ended up with no completions
            // The input was at least partially parsed and succeded, so its an inconsistency.
            return CompletionResult::Inconsistency(
                "No completions available from partial AST.".to_string(),
            );
        }
        
        debug_debug!(
            "complete_ast",
            "At depth {}: input='{}' -> completions: {:?}",
            current_state.depth,
            current_state.input,
            all_raw_completions
        );
        
        debug_debug!(
            "complete_ast",
            "valid_tokens: {:?}",
            valid_tokens
        );

        // If we haven't reached max depth, explore next completion tokens
        if current_state.depth < max_depth {
            for token in valid_tokens {
                let next_input = match extend_input(&current_state.input, &token) {
                    Ok(input) => input,
                    Err(e) => return CompletionResult::Inconsistency(e),
                };

                // Skip if we've already explored this input
                // this is tree convergence pruning
                if visited.contains(&next_input) {
                    continue;
                }

                let mut next_path = current_state.path.clone();
                next_path.push(token.clone());

                queue.push_back(CompletionState {
                    input: next_input,
                    depth: current_state.depth + 1,
                    path: next_path,
                });
            }
        }
    }

    CompletionResult::Failure {
        max_depth_reached: max_depth,
        states_explored,
        visited_states: visited.into_iter().collect(),
    }
}

/// Filters the set of syntactic completions based on typing constraints.
///
/// This function implements the "Type Filtering" logic described in `docs/completion_logic.md`.
/// It identifies the hole in the partial AST, collects the available context (bound variables),
/// and determines if the hole is constrained by a typing rule (e.g., must be a variable).
///
/// # Arguments
/// * `grammar` - The grammar definition.
/// * `root` - The current partial AST root.
/// * `completions` - The set of syntactically valid completion tokens.
///
/// # Returns
/// A vector of `DerivativeRegex` tokens that are valid in the current typing context.
pub fn type_filtering(
    grammar: &Grammar,
    root: &crate::logic::partial::NonTerminal,
    completions: &[DerivativeRegex],
    initial_vars: &[String],
) -> Vec<DerivativeRegex> {
    let mut context_vars = Vec::new();
    let mut is_constrained = false;

    // Collect context from this root
    if let Some((vars, constrained)) = collect_context_at_hole(
        &Node::NonTerminal(root.clone()),
        grammar,
        initial_vars.to_vec(),
        false,
    ) {
        context_vars = vars;
        is_constrained = constrained;
    }

    let mut valid_tokens = Vec::new();

    if is_constrained && !context_vars.is_empty() {
        debug_debug!("type_filtering", "Constrained mode. Context: {:?}", context_vars);
        // Constrained mode: Only allow variables from the context that match the syntax
        for var in &context_vars {
            let var_token = DerivativeRegex::literal(var);
            
            // Check if this variable is syntactically allowed
            let matches_syntax = completions.iter().any(|t| t.match_full(var));
            
            if matches_syntax {
                valid_tokens.push(var_token);
            }
        }
    } else {
        // Unconstrained mode OR constrained with empty context (fall back to syntax)
        // Unconstrained mode: Allow all syntactic completions + context variables
        
        // 1. Add all syntactic completions
        valid_tokens.extend_from_slice(completions);

        // 2. Add context variables as suggestions (if they match any syntactic token)
        for var in &context_vars {
             let matches_syntax = completions.iter().any(|t| t.match_full(var));
             if matches_syntax {
                 valid_tokens.push(DerivativeRegex::literal(var));
             }
        }
    }
    
    valid_tokens
}

/// Try to parse the input as a complete AST (not partial)

/// Extend input string with a completion token
fn extend_input(input: &str, token: &DerivativeRegex) -> Result<String, String> {
    match token.example() {
        Some(e) => {
            // Add a space before the token if the input is non-empty and doesn't already end with whitespace
            let first = e.chars().next().unwrap_or(' ');
            if input.is_empty()
                || input.ends_with(' ')
                || input.ends_with('\n')
                || input.ends_with('\t')
                || !first.is_alphanumeric()
            {
                Ok(format!("{}{}", input, e))
            } else {
                Ok(format!("{} {}", input, e))
            }
        }
        None => {
            // Empty regex - this shouldn't happen for valid completion tokens
            // Skip this token by returning the input unchanged
            Ok(input.to_string())
        }
    }
}

/// Test helper: Check if a grammar and input combination is completable
pub fn is_completable(grammar: &Grammar, input: &str, max_depth: usize) -> bool {
    matches!(
        complete_ast(grammar, input, max_depth),
        CompletionResult::Success { .. }
    )
}

/// Test helper: Get completion statistics
pub fn completion_stats(grammar: &Grammar, input: &str, max_depth: usize) -> (bool, usize, usize) {
    match complete_ast(grammar, input, max_depth) {
        CompletionResult::Success { depth, .. } => (true, depth, 1),
        CompletionResult::Failure {
            states_explored, ..
        } => (false, max_depth, states_explored),
        CompletionResult::Error(_) => (false, 0, 0),
        CompletionResult::Invalid(_) => (false, 0, 0),
        CompletionResult::Inconsistency(_) => (false, 0, 0),
    }
}

fn get_node_text(node: &Node) -> Option<String> {
    match node {
        Node::Terminal(Terminal::Complete { value, .. }) => Some(value.clone()),
        Node::Terminal(Terminal::Partial { .. }) => None,
        Node::NonTerminal(nt) => {
            let mut s = String::new();
            for child in &nt.children {
                if let Some(text) = get_node_text(child) {
                    s.push_str(&text);
                } else {
                    return None;
                }
            }
            Some(s)
        }
    }
}

fn collect_context_at_hole(
    node: &Node,
    grammar: &Grammar,
    current_ctx: Vec<String>,
    inherited_constraint: bool,
) -> Option<(Vec<String>, bool)> {
    match node {
        Node::Terminal(Terminal::Partial { .. }) => Some((current_ctx, inherited_constraint)),
        Node::Terminal(Terminal::Complete { .. }) => None,
        Node::NonTerminal(nt) => {
            if nt.is_complete() {
                return None;
            }

            // Calculate new context if this node has a rule
            let mut new_ctx = current_ctx.clone();
            if let Some(rule_name) = &nt.production.rule {
                if let Some(rule) = grammar.typing_rules.get(rule_name) {
                    // Check premises for context extensions
                    for premise in &rule.premises {
                        if let Some(setting) = &premise.setting {
                            for (var_name, _) in &setting.extensions {
                                // Resolve var_name to a node
                                if let Some(paths) = grammar.binding_map.get(var_name, rule_name) {
                                    for path in paths {
                                        if let Ok(results) = resolve_binding_path(node, path) {
                                            if let Some(res) = results.first() {
                                                if let Some(text) = get_node_text(res.node()) {
                                                    new_ctx.push(text);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Recurse into children
            for (i, child) in nt.children.iter().enumerate() {
                let mut child_constraint = false;

                // Determine if this child is constrained by the current rule
                if let Some(rule_name) = &nt.production.rule {
                    if let Some(rule) = grammar.typing_rules.get(rule_name) {
                        if let Some(expr) = nt.production.rhs.get(i) {
                            if let Some(binding_name) = expr.binding() {
                                for premise in &rule.premises {
                                    if let Some(TypingJudgment::Membership(v, _)) =
                                        &premise.judgment
                                    {
                                        if v == binding_name {
                                            child_constraint = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                let effective_constraint = inherited_constraint || child_constraint;

                if let Some(result) = collect_context_at_hole(
                    child,
                    grammar,
                    new_ctx.clone(),
                    effective_constraint,
                ) {
                    return Some(result);
                }
            }
            None
        }
    }
}
