
use super::typing::{
    BoundConclusion, BoundConclusionContext, BoundConclusionKind, BoundPremise, BoundType,
    BoundTypeAscription, BoundTypeSetting, BoundTypingJudgment, BoundTypingRule,
};
use crate::logic::grammar::{Grammar, Symbol};
use crate::logic::partial::{Alt, NonTerminal, ParsedNode, Slot};
use crate::logic::typing::rule::{ConclusionKind, Premise, TypingJudgment};
use crate::logic::typing::{Type, TypingRule};
use std::collections::HashMap;

/// Result of binding validation for a partial AST
#[derive(Debug, Clone)]
pub struct BindingResult {
    /// Whether all required bindings are satisfied (present or completable)
    pub valid: bool,
    /// Bindings that are present in parsed nodes
    pub present: HashMap<String, BindingInfo>,
    /// Bindings that are missing but could appear through completion
    pub completable: HashMap<String, CompletableBinding>,
    /// Bindings that are missing and cannot be satisfied
    pub missing: Vec<String>,
    /// Type constraints extracted from the rule
    pub constraints: Vec<TypeConstraint>,
}

/// Information about a present binding
#[derive(Debug, Clone)]
pub struct BindingInfo {
    /// The binding name (e.g., "x", "e")
    pub name: String,
    /// Symbol index in the production
    pub symbol_index: usize,
    /// Path to the node (for nested groups)
    pub path: Vec<usize>,
    /// The parsed node that carries this binding
    pub node: ParsedNode,
}

/// Information about a completable binding
#[derive(Debug, Clone)]
pub struct CompletableBinding {
    /// The binding name
    pub name: String,
    /// Symbol index where this binding should appear
    pub symbol_index: usize,
    /// The production symbol that should carry this binding
    pub symbol: Symbol,
}

/// Type constraint from premises or conclusion
#[derive(Debug, Clone)]
pub enum TypeConstraint {
    /// Direct type ascription (term : type)
    Ascription { binding: String, ty: Type },
    /// Context membership (x ∈ Γ)
    Membership { binding: String, context: String },
    /// Context extension Γ[x:τ]
    Extension {
        context: String,
        binding: String,
        ty: Type,
    },
}

impl NonTerminal {
    /// Bind a flattened NonTerminal (must have exactly one alternative)
    pub fn bind(&mut self, grammar: &Grammar) -> Result<BindingResult, String> {
        if self.alts.len() != 1 {
            return Err(format!(
                "NonTerminal '{}' must be flattened before binding (has {} alts)",
                self.name,
                self.alts.len()
            ));
        }

        let alt = &mut self.alts[0];
        bind_alternative(alt, grammar)
    }
}

/// Main binding function for a single alternative
pub fn bind_alternative(alt: &mut Alt, grammar: &Grammar) -> Result<BindingResult, String> {
    // Get the typing rule if present
    let rule_name = match &alt.production.rule {
        Some(name) => name,
        None => {
            // No typing rule - create empty binding result
            return Ok(BindingResult {
                valid: true,
                present: HashMap::new(),
                completable: HashMap::new(),
                missing: Vec::new(),
                constraints: Vec::new(),
            });
        }
    };

    let typing_rule = grammar
        .typing_rules
        .get(rule_name)
        .ok_or_else(|| format!("Typing rule '{}' not found in grammar", rule_name))?;

    // Extract required bindings from the rule
    let required_bindings = extract_required_bindings(typing_rule);

    // Scan for present bindings in parsed nodes
    let present = scan_present_bindings(alt)?;

    // Check which required bindings are completable
    let (completable, missing) =
        check_completable(&required_bindings, &present, alt, &alt.production);

    // Extract type constraints from the rule
    let constraints = extract_type_constraints(typing_rule);

    // Apply type constraints to slots
    apply_type_constraints(alt, &constraints, &present, &completable);

    // Create bound rule
    let bound_rule = create_bound_rule(typing_rule, &present, &completable)?;

    // Store bound rule in the alt
    if let Some(ref mut rule) = alt.typing_rule {
        rule.bound = Some(bound_rule);
    }

    let valid = missing.is_empty();

    Ok(BindingResult {
        valid,
        present,
        completable,
        missing,
        constraints,
    })
}

/// Extract all binding names referenced in a typing rule
fn extract_required_bindings(rule: &TypingRule) -> Vec<String> {
    let mut bindings = Vec::new();

    // From premises
    for premise in &rule.premises {
        if let Some(setting) = &premise.setting {
            for (term, _ty) in &setting.extensions {
                if !bindings.contains(term) {
                    bindings.push(term.clone());
                }
            }
        }
        if let Some(judgment) = &premise.judgment {
            match judgment {
                TypingJudgment::Ascription((term, _)) => {
                    if !bindings.contains(term) {
                        bindings.push(term.clone());
                    }
                }
                TypingJudgment::Membership(var, _) => {
                    if !bindings.contains(var) {
                        bindings.push(var.clone());
                    }
                }
            }
        }
    }

    // From conclusion (context lookups reference bindings)
    match &rule.conclusion.kind {
        ConclusionKind::ContextLookup(_ctx, var) => {
            if !bindings.contains(var) {
                bindings.push(var.clone());
            }
        }
        _ => {}
    }

    bindings
}

/// Scan all slots for present bindings
fn scan_present_bindings(alt: &Alt) -> Result<HashMap<String, BindingInfo>, String> {
    let mut present = HashMap::new();

    // Get sorted slot indices
    let mut indices: Vec<usize> = alt.slots.keys().copied().collect();
    indices.sort_unstable();

    for idx in indices {
        if let Some(slot) = alt.slots.get(&idx) {
            // Get the binding from the production symbol if present
            let symbol_binding = alt.production.rhs.get(idx).and_then(|sym| sym.binding().cloned());
            scan_slot_bindings(slot, idx, &vec![], &mut present, symbol_binding.as_deref())?;
        }
    }

    Ok(present)
}

/// Recursively scan a slot for bindings
fn scan_slot_bindings(
    slot: &Slot,
    symbol_index: usize,
    path: &[usize],
    present: &mut HashMap<String, BindingInfo>,
    symbol_binding: Option<&str>,
) -> Result<(), String> {
    match slot {
        Slot::Filled { nodes, .. } => {
            for node in nodes {
                // First check if the production symbol has a binding
                let node_binding = get_node_binding(node)?;
                let binding = symbol_binding
                    .map(|s| s.to_string())
                    .or(node_binding);
                    
                if let Some(binding) = binding {
                    present.insert(
                        binding.clone(),
                        BindingInfo {
                            name: binding,
                            symbol_index,
                            path: path.to_vec(),
                            node: node.clone(),
                        },
                    );
                }
            }
        }
        Slot::Partial { node: Some(n), .. } => {
            let node_binding = get_node_binding(n)?;
            let binding = symbol_binding
                .map(|s| s.to_string())
                .or(node_binding);
                
            if let Some(binding) = binding {
                present.insert(
                    binding.clone(),
                    BindingInfo {
                        name: binding,
                        symbol_index,
                        path: path.to_vec(),
                        node: n.clone(),
                    },
                );
            }
        }
        Slot::Group {
            iterations,
            partial_iteration,
            ..
        } => {
            // Scan complete iterations
            for (iter_idx, iteration) in iterations.iter().enumerate() {
                for (slot_idx, inner_slot) in iteration.iter().enumerate() {
                    let mut new_path = path.to_vec();
                    new_path.push(iter_idx);
                    new_path.push(slot_idx);
                    scan_slot_bindings(inner_slot, symbol_index, &new_path, present, symbol_binding)?;
                }
            }
            // Scan partial iteration if present
            if let Some(partial) = partial_iteration {
                for (slot_idx, inner_slot) in partial.iter().enumerate() {
                    let mut new_path = path.to_vec();
                    new_path.push(iterations.len()); // Use next iteration index
                    new_path.push(slot_idx);
                    scan_slot_bindings(inner_slot, symbol_index, &new_path, present, symbol_binding)?;
                }
            }
        }
        _ => {}
    }
    Ok(())
}

/// Get binding name from a parsed node
/// Returns an error if a terminal has a binding (which is invalid)
fn get_node_binding(node: &ParsedNode) -> Result<Option<String>, String> {
    match node {
        ParsedNode::Terminal(t) => {
            if t.binding.is_some() {
                Err(format!("Terminal '{}' cannot have a binding", t.value))
            } else {
                Ok(None)
            }
        }
        ParsedNode::NonTerminal(nt) => Ok(nt.binding.clone()),
    }
}

/// Check which required bindings are completable
fn check_completable(
    required: &[String],
    present: &HashMap<String, BindingInfo>,
    alt: &Alt,
    production: &crate::logic::grammar::Production,
) -> (HashMap<String, CompletableBinding>, Vec<String>) {
    let cursor = alt.cursor();
    let mut completable = HashMap::new();
    let mut missing = Vec::new();

    for binding_name in required {
        if present.contains_key(binding_name) {
            continue; // Already present
        }

        // Search in unparsed symbols
        let mut found = false;
        for idx in cursor..production.rhs.len() {
            if symbol_has_binding(&production.rhs[idx], binding_name) {
                completable.insert(
                    binding_name.clone(),
                    CompletableBinding {
                        name: binding_name.clone(),
                        symbol_index: idx,
                        symbol: production.rhs[idx].clone(),
                    },
                );
                found = true;
                break;
            }
        }

        if !found {
            missing.push(binding_name.clone());
        }
    }

    (completable, missing)
}

/// Check if a symbol has a specific binding (recursively for Single and Group)
fn symbol_has_binding(symbol: &Symbol, binding_name: &str) -> bool {
    match symbol {
        Symbol::Single { value, binding, .. } => {
            if binding.as_deref() == Some(binding_name) {
                return true;
            }
            symbol_has_binding(value, binding_name)
        }
        Symbol::Group { symbols, .. } => {
            symbols.iter().any(|s| symbol_has_binding(s, binding_name))
        }
        _ => false,
    }
}

/// Extract type constraints from a typing rule
fn extract_type_constraints(rule: &TypingRule) -> Vec<TypeConstraint> {
    let mut constraints = Vec::new();

    for premise in &rule.premises {
        // Context extensions
        if let Some(setting) = &premise.setting {
            for (term, ty) in &setting.extensions {
                constraints.push(TypeConstraint::Extension {
                    context: setting.name.clone(),
                    binding: term.clone(),
                    ty: ty.clone(),
                });
            }
        }

        // Judgments
        if let Some(judgment) = &premise.judgment {
            match judgment {
                TypingJudgment::Ascription((term, ty)) => {
                    constraints.push(TypeConstraint::Ascription {
                        binding: term.clone(),
                        ty: ty.clone(),
                    });
                }
                TypingJudgment::Membership(var, ctx) => {
                    constraints.push(TypeConstraint::Membership {
                        binding: var.clone(),
                        context: ctx.clone(),
                    });
                }
            }
        }
    }

    constraints
}

/// Apply type constraints to slots
fn apply_type_constraints(
    alt: &mut Alt,
    constraints: &[TypeConstraint],
    present: &HashMap<String, BindingInfo>,
    completable: &HashMap<String, CompletableBinding>,
) {
    for constraint in constraints {
        match constraint {
            TypeConstraint::Ascription { binding, ty } => {
                // Find the slot for this binding
                let symbol_index = if let Some(info) = present.get(binding) {
                    Some(info.symbol_index)
                } else if let Some(comp) = completable.get(binding) {
                    Some(comp.symbol_index)
                } else {
                    None
                };

                if let Some(idx) = symbol_index {
                    if let Some(slot) = alt.slots.get_mut(&idx) {
                        set_slot_type_constraint(slot, ty.clone());
                    }
                }
            }
            TypeConstraint::Extension { binding, ty, .. } => {
                // Same as ascription for now
                let symbol_index = if let Some(info) = present.get(binding) {
                    Some(info.symbol_index)
                } else if let Some(comp) = completable.get(binding) {
                    Some(comp.symbol_index)
                } else {
                    None
                };

                if let Some(idx) = symbol_index {
                    if let Some(slot) = alt.slots.get_mut(&idx) {
                        set_slot_type_constraint(slot, ty.clone());
                    }
                }
            }
            TypeConstraint::Membership { .. } => {
                // Membership constraints don't directly set slot types
                // They're used in the bound rule premises
            }
        }
    }
}

/// Set type constraint on a slot
fn set_slot_type_constraint(slot: &mut Slot, ty: Type) {
    match slot {
        Slot::Filled {
            type_constraint, ..
        } => {
            *type_constraint = Some(ty);
        }
        Slot::Partial {
            type_constraint, ..
        } => {
            *type_constraint = Some(ty);
        }
        Slot::Group { .. } => {
            // Groups don't have direct type constraints
            // Type constraints are applied to inner slots
        }
    }
}

/// Create a bound typing rule from the unbound rule
fn create_bound_rule(
    rule: &TypingRule,
    present: &HashMap<String, BindingInfo>,
    completable: &HashMap<String, CompletableBinding>,
) -> Result<BoundTypingRule, String> {
    // Convert premises
    let bound_premises: Vec<BoundPremise> = rule
        .premises
        .iter()
        .map(|p| bind_premise(p, present, completable))
        .collect::<Result<Vec<_>, _>>()?;

    // Convert conclusion
    let bound_conclusion = bind_conclusion(&rule.conclusion, present, completable)?;

    Ok(BoundTypingRule {
        name: rule.name.clone(),
        premises: bound_premises,
        conclusion: bound_conclusion,
    })
}

/// Bind a premise
fn bind_premise(
    premise: &Premise,
    present: &HashMap<String, BindingInfo>,
    _completable: &HashMap<String, CompletableBinding>,
) -> Result<BoundPremise, String> {
    let bound_setting = if let Some(setting) = &premise.setting {
        let mut extensions = Vec::new();
        for (term, ty) in &setting.extensions {
            let node = if let Some(info) = present.get(term) {
                match &info.node {
                    ParsedNode::NonTerminal(nt) => Some(nt.clone()),
                    ParsedNode::Terminal(t) => {
                        return Err(format!(
                            "Binding '{}' on terminal '{}' is invalid - terminals cannot carry bindings",
                            term, t.value
                        ));
                    }
                }
            } else {
                // Binding not present yet (completable or missing)
                None
            };
            
            extensions.push(BoundTypeAscription {
                node,
                ty: type_to_bound_type(ty),
            });
        }
        
        Some(BoundTypeSetting {
            name: setting.name.clone(),
            extensions,
        })
    } else {
        None
    };

    let bound_judgment = if let Some(judgment) = &premise.judgment {
        Some(match judgment {
            TypingJudgment::Ascription((term, ty)) => {
                let node = if let Some(info) = present.get(term) {
                    match &info.node {
                        ParsedNode::NonTerminal(nt) => Some(nt.clone()),
                        ParsedNode::Terminal(t) => {
                            return Err(format!(
                                "Binding '{}' on terminal '{}' is invalid - terminals cannot carry bindings",
                                term, t.value
                            ));
                        }
                    }
                } else {
                    None
                };
                
                BoundTypingJudgment::Ascription(BoundTypeAscription {
                    node,
                    ty: type_to_bound_type(ty),
                })
            }
            TypingJudgment::Membership(_var, ctx) => {
                // For membership, we don't bind to a specific node
                BoundTypingJudgment::Membership(None, ctx.clone())
            }
        })
    } else {
        None
    };

    Ok(BoundPremise {
        setting: bound_setting,
        judgment: bound_judgment,
    })
}

/// Bind a conclusion
fn bind_conclusion(
    conclusion: &crate::logic::typing::Conclusion,
    present: &HashMap<String, BindingInfo>,
    _completable: &HashMap<String, CompletableBinding>,
) -> Result<BoundConclusion, String> {
    let bound_context = BoundConclusionContext {
        input: conclusion.context.input.clone(),
        output: if let Some(setting) = &conclusion.context.output {
            let mut extensions = Vec::new();
            for (term, ty) in &setting.extensions {
                let node = if let Some(info) = present.get(term) {
                    match &info.node {
                        ParsedNode::NonTerminal(nt) => Some(nt.clone()),
                        ParsedNode::Terminal(t) => {
                            return Err(format!(
                                "Binding '{}' on terminal '{}' is invalid in conclusion context",
                                term, t.value
                            ));
                        }
                    }
                } else {
                    None
                };
                
                extensions.push(BoundTypeAscription {
                    node,
                    ty: type_to_bound_type(ty),
                });
            }
            
            Some(BoundTypeSetting {
                name: setting.name.clone(),
                extensions,
            })
        } else {
            None
        },
    };

    let bound_kind = match &conclusion.kind {
        ConclusionKind::Type(ty) => BoundConclusionKind::Type(type_to_bound_type(ty)),
        ConclusionKind::ContextLookup(ctx, var) => {
            let node = if let Some(info) = present.get(var) {
                match &info.node {
                    ParsedNode::NonTerminal(nt) => Some(nt.clone()),
                    ParsedNode::Terminal(t) => {
                        return Err(format!(
                            "Context lookup for binding '{}' on terminal '{}' is invalid",
                            var, t.value
                        ));
                    }
                }
            } else {
                None
            };
            BoundConclusionKind::ContextLookup(ctx.clone(), node)
        }
    };

    Ok(BoundConclusion {
        context: bound_context,
        kind: bound_kind,
    })
}

/// Convert Type to BoundType
fn type_to_bound_type(ty: &Type) -> BoundType {
    match ty {
        Type::Atom(s) => BoundType::Atom(s.clone()),
        Type::Raw(s) => BoundType::Atom(s.clone()), // Treat raw types as atoms
        Type::Arrow(a, b) => BoundType::Arrow(
            Box::new(type_to_bound_type(a)),
            Box::new(type_to_bound_type(b)),
        ),
        Type::Tuple(t) => BoundType::Atom(t.clone()), // Represent tuple as atom for now
        Type::Not(t) => BoundType::Not(Box::new(type_to_bound_type(t))),
        Type::Intersection(a, b) => BoundType::Intersection(
            Box::new(type_to_bound_type(a)),
            Box::new(type_to_bound_type(b)),
        ),
        Type::Union(a, b) => BoundType::Union(
            Box::new(type_to_bound_type(a)),
            Box::new(type_to_bound_type(b)),
        ),
        Type::ContextCall(ctx, var) => BoundType::ContextCall(ctx.clone(), var.clone()),
        Type::Universe => BoundType::Universe,
        Type::Empty => BoundType::Empty,
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_bind_simple_lambda() {
        let spec = r#"
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Variable(var) ::= Identifier[x]
        TypeName ::= Identifier
        Type ::= TypeName[τ]
        Term ::= Variable[e]
        Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ] '.' Term[e]

        Γ[x:τ] ⊢ e : τ₂
        --------------------------- (lambda)
        τ → τ₂
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::Parser::new(g.clone());
        
        // Parse partial lambda
        let partial = p.partial("λ x :").unwrap();
        let mut forest = partial.root.into_forest();
        
        assert!(forest.len() > 0, "Should have at least one tree");
        
        for tree in &mut forest {
            println!("Tree: {}", tree.name);
            let result = tree.bind(&g);
            
            match result {
                Ok(binding_result) => {
                    println!("Present: {:?}", binding_result.present.keys());
                    println!("Completable: {:?}", binding_result.completable.keys());
                    println!("Missing: {:?}", binding_result.missing);
                    println!("Valid: {}", binding_result.valid);
                }
                Err(e) => {
                    println!("Binding error: {}", e);
                }
            }
        }
    }

    #[test]
    fn test_bind_completable() {
        let spec = r#"
        A ::= 'a'
        B(rule) ::= A[x] 'b'

        x ∈ Γ
        ----------- (rule)
        Γ(x)
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        
        // Check the production structure
        let b_prod = &g.productions.get("B").unwrap()[0];
        println!("B production symbols:");
        for (idx, sym) in b_prod.rhs.iter().enumerate() {
            println!("  [{}] {:?}", idx, sym);
            println!("      binding: {:?}", sym.binding());
        }
        
        let mut p = crate::logic::partial::Parser::new(g.clone());
        
        // Parse complete B
        let partial = p.partial("a b").unwrap();
        println!("\nPartial AST: {:#?}", partial.root);
        
        let mut forest = partial.root.into_forest();
        
        println!("\nForest size: {}", forest.len());
        for tree in &mut forest {
            println!("Tree: {}", tree.name);
            println!("Alt production: {:?}", tree.alts[0].production);
            println!("Slots:");
            for (idx, slot) in &tree.alts[0].slots {
                println!("  [{}] {:?}", idx, slot);
            }
            
            if tree.name == "B" {
                let result = tree.bind(&g);
                match &result {
                    Ok(br) => {
                        println!("\nBinding result:");
                        println!("Valid: {}", br.valid);
                        println!("Present bindings: {:?}", br.present.keys().collect::<Vec<_>>());
                        println!("Completable bindings: {:?}", br.completable.keys().collect::<Vec<_>>());
                        println!("Missing bindings: {:?}", br.missing);
                    }
                    Err(e) => {
                        println!("Error: {}", e);
                        panic!("Binding failed: {}", e);
                    }
                }
                let br = result.unwrap();
                assert!(br.valid, "Should be valid with present binding");
                assert!(br.present.contains_key("x"), "Should have x present");
                assert!(br.completable.is_empty(), "No completable bindings");
                assert!(br.missing.is_empty(), "No missing bindings");
            }
        }
    }

    #[test]
    fn test_terminal_binding_fails() {
        // This test ensures that if somehow a terminal gets a binding, 
        // we properly reject it
        let spec = r#"
A ::= 'a'
B(rule) ::= A[x] 'b'

x ∈ Γ
----------- (rule)
Γ(x)
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::Parser::new(g.clone());
        
        let mut partial = p.partial("a b").unwrap();
        
        // Manually corrupt the AST to add a binding to a terminal
        // This simulates a bug or invalid state
        if let Some(alt) = partial.root.alts.first_mut() {
            if let Some(crate::logic::partial::Slot::Filled { nodes, .. }) = alt.slots.get_mut(&1) {
                if let Some(crate::logic::partial::ParsedNode::Terminal(t)) = nodes.first_mut() {
                    t.binding = Some("invalid".to_string());
                }
            }
        }
        
        let mut forest = partial.root.into_forest();
        
        for tree in &mut forest {
            if tree.name == "B" {
                let result = tree.bind(&g);
                assert!(result.is_err(), "Should fail when terminal has binding");
                if let Err(e) = result {
                    assert!(e.contains("Terminal") && e.contains("cannot have a binding"),
                            "Error message should mention terminal binding issue: {}", e);
                }
            }
        }
    }

    #[test]
    fn test_arithmetic_expression_binding() {
        // Simple grammar with proper typing rules
        let spec = r#"
Number ::= /[0-9]+/
Var ::= /[a-z]+/

Lit(lit) ::= Number[n]
Add(add) ::= Lit[e1] '+' Lit[e2]

----------- (lit)
int

Γ ⊢ e1 : int, Γ ⊢ e2 : int
---------------------------- (add)
int
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        println!("\n=== Grammar Loaded ===");
        println!("Productions: {:?}", g.productions.keys().collect::<Vec<_>>());
        println!("Typing rules: {:?}", g.typing_rules.keys().collect::<Vec<_>>());
        
        assert_eq!(g.typing_rules.len(), 2, "Should have 2 typing rules");
        assert!(g.typing_rules.contains_key("lit"), "Should have 'lit' rule");
        assert!(g.typing_rules.contains_key("add"), "Should have 'add' rule");
        
        let mut p = crate::logic::partial::Parser::new(g.clone());
        
        println!("\n=== Test 1: Simple Literal '42' ===");
        let partial = p.partial("42").unwrap();
        println!("Root: {}", partial.root.name);
        
        let mut forest = partial.root.into_forest();
        println!("Forest size: {}", forest.len());
        
        //  trees with Lit nonterminal 
        let mut found_lit = false;
        for tree in &mut forest {
            println!("\nChecking tree: {}", tree.name);
            // Look for Lit either at top level or in the first slot
            let lit_tree = if tree.name == "Lit" {
                Some(tree)
            } else if let Some(alt) = tree.alts.first_mut() {
                // Check first slot for Lit
                if let Some(crate::logic::partial::Slot::Filled { nodes, .. }) = alt.slots.get_mut(&0) {
                    nodes.iter_mut().find_map(|node| match node {
                        crate::logic::partial::ParsedNode::NonTerminal(nt) if nt.name == "Lit" => Some(nt),
                        _ => None,
                    })
                } else {
                    None
                }
            } else {
                None
            };
            
            if let Some(lit) = lit_tree {
                found_lit = true;
                println!("Found Lit!");
                let result = lit.bind(&g);
                match &result {
                    Ok(br) => {
                        println!("Valid: {}", br.valid);
                        println!("Present: {:?}", br.present.keys().collect::<Vec<_>>());
                        println!("Completable: {:?}", br.completable.keys().collect::<Vec<_>>());
                        println!("Missing: {:?}", br.missing);
                        assert!(br.valid, "Literal should have valid bindings");
                        assert!(br.present.contains_key("n"), "Should have 'n' binding for number");
                        
                        // Check bound rule was created
                        if let Some(alt) = lit.alts.first() {
                            if let Some(rule) = &alt.typing_rule {
                                println!("Rule: {}", rule.name);
                                assert_eq!(rule.name, "lit");
                                assert!(rule.bound.is_some(), "Should have bound rule");
                            }
                        }
                    }
                    Err(e) => panic!("Binding failed: {}", e),
                }
                break;
            }
        }
        assert!(found_lit, "Should have found Lit in forest or nested in Add");
        
        //  Parse a complete addition
        println!("\n=== Test 2: Complete Addition '1 + 2' ===");
        let partial = p.partial("1 + 2").unwrap();
        println!("Root: {}", partial.root.name);
        
        let mut forest = partial.root.into_forest();
        println!("Forest size: {}", forest.len());
        
        let mut found_add = false;
        for tree in &mut forest {
            println!("\nTree: {}", tree.name);
            if tree.name == "Add" {
                found_add = true;
                let result = tree.bind(&g);
                match &result {
                    Ok(br) => {
                        println!("Valid: {}", br.valid);
                        println!("Present: {:?}", br.present.keys().collect::<Vec<_>>());
                        println!("Completable: {:?}", br.completable.keys().collect::<Vec<_>>());
                        println!("Missing: {:?}", br.missing);
                        
                        assert!(br.valid, "Complete addition should be valid");
                        assert!(br.present.contains_key("e1"), "Should have e1 present");
                        assert!(br.present.contains_key("e2"), "Should have e2 present");
                        assert!(br.completable.is_empty(), "No completable bindings in complete parse");
                        assert!(br.missing.is_empty(), "No missing bindings");
                        
                        // Check type constraints were applied
                        if let Some(alt) = tree.alts.first() {
                            println!("\n  Checking slots:");
                            for (idx, slot) in &alt.slots {
                                match slot {
                                    crate::logic::partial::Slot::Filled { type_constraint, .. } => {
                                        println!("    Slot {}: type = {:?}", idx, type_constraint);
                                        if *idx == 0 || *idx == 2 {
                                            // e1 and e2 slots should have type constraints
                                            assert!(type_constraint.is_some(), 
                                                   "Slot {} should have type constraint", idx);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            
                            // Check bound rule
                            if let Some(rule) = &alt.typing_rule {
                                println!("\n  Rule: {}", rule.name);
                                assert_eq!(rule.name, "add");
                                assert!(rule.bound.is_some(), "Should have bound rule");
                                
                                if let Some(bound) = &rule.bound {
                                    println!("  Premises: {}", bound.premises.len());
                                    assert_eq!(bound.premises.len(), 2, "Add rule should have 2 premises");
                                }
                            }
                        }
                    }
                    Err(e) => panic!("Binding failed: {}", e),
                }
            }
        }
        assert!(found_add, "Should have found Add in forest");
        
        // partial addition
        println!("\n=== Test 3: Partial Addition '1 +' ===");
        let partial = p.partial("1 +").unwrap();
        let mut forest = partial.root.into_forest();
        
        for tree in &mut forest {
            if tree.name == "Add" {
                let result = tree.bind(&g);
                match &result {
                    Ok(br) => {
                        println!("Valid: {}", br.valid);
                        println!("Present: {:?}", br.present.keys().collect::<Vec<_>>());
                        println!("Completable: {:?}", br.completable.keys().collect::<Vec<_>>());
                        println!("Missing: {:?}", br.missing);
                        
                        // The partial parser might complete with an empty match, 
                        // or mark e2 as completable
                        // we should calrify this behavior
                        assert!(br.valid, "Partial should be valid");
                        assert!(br.present.contains_key("e1"), "e1 should be present");
                        // e2 could be either present (with partial) or completable
                        assert!(br.present.contains_key("e2") || br.completable.contains_key("e2"), 
                               "e2 should be present or completable");
                    }
                    Err(e) => panic!("Binding failed: {}", e),
                }
                break;
            }
        }
    }
}

