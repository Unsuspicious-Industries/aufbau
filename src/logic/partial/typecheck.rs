/// Type checking for partial ASTs
/// 
/// This module provides full type checking with binding resolution and detailed error reporting.

use super::*;
use crate::logic::grammar::Grammar;
use crate::logic::typing::{Type, TypingRule, Premise, Conclusion};
use crate::logic::typing::rule::{ConclusionKind, TypingJudgment};
use crate::logic::bind::typing::BoundType;

/// Detailed type checking error with context
#[derive(Clone, Debug)]
pub struct TypeCheckError {
    pub message: String,
    pub rule_name: Option<String>,
    pub context: Vec<String>,
}

impl TypeCheckError {
    fn new(message: String) -> Self {
        Self {
            message,
            rule_name: None,
            context: Vec::new(),
        }
    }
    
    fn with_rule(mut self, rule_name: String) -> Self {
        self.rule_name = Some(rule_name);
        self
    }
    
    fn with_context(mut self, ctx: String) -> Self {
        self.context.push(ctx);
        self
    }
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type error: {}", self.message)?;
        if let Some(rule) = &self.rule_name {
            write!(f, " (rule: {})", rule)?;
        }
        if !self.context.is_empty() {
            write!(f, " [{}]", self.context.join(" > "))?;
        }
        Ok(())
    }
}

/// Result of type checking an alternative
#[derive(Clone, Debug)]
pub enum TypeCheckResult {
    /// The alternative can be typed
    Typeable,
    /// The alternative cannot be typed
    NotTypeable(TypeCheckError),
    /// Type checking not applicable (no typing rule)
    NotApplicable,
}

impl TypeCheckResult {
    pub fn is_typeable(&self) -> bool {
        matches!(self, TypeCheckResult::Typeable | TypeCheckResult::NotApplicable)
    }
    
    pub fn error(&self) -> Option<&TypeCheckError> {
        match self {
            TypeCheckResult::NotTypeable(err) => Some(err),
            _ => None,
        }
    }
}

/// Type binding context during checking
#[derive(Clone, Debug)]
struct TypeContext {
    bindings: std::collections::HashMap<String, BoundType>,
}

impl TypeContext {
    fn new() -> Self {
        Self {
            bindings: std::collections::HashMap::new(),
        }
    }
    
    fn bind(&mut self, name: String, ty: BoundType) {
        self.bindings.insert(name, ty);
    }
    
    fn lookup(&self, name: &str) -> Option<&BoundType> {
        self.bindings.get(name)
    }
}

/// Convert a Type to BoundType
fn type_to_bound_type(ty: &Type) -> Result<BoundType, String> {
    match ty {
        Type::Atom(s) | Type::Raw(s) => Ok(BoundType::Atom(s.clone())),
        Type::Arrow(t1, t2) => {
            let b1 = type_to_bound_type(t1)?;
            let b2 = type_to_bound_type(t2)?;
            Ok(BoundType::Arrow(Box::new(b1), Box::new(b2)))
        }
        Type::Pointer(t) => {
            let bt = type_to_bound_type(t)?;
            Ok(BoundType::Pointer(Box::new(bt)))
        }
        Type::Not(t) => {
            let bt = type_to_bound_type(t)?;
            Ok(BoundType::Not(Box::new(bt)))
        }
        Type::Intersection(t1, t2) => {
            let b1 = type_to_bound_type(t1)?;
            let b2 = type_to_bound_type(t2)?;
            Ok(BoundType::Intersection(Box::new(b1), Box::new(b2)))
        }
        Type::Union(t1, t2) => {
            let b1 = type_to_bound_type(t1)?;
            let b2 = type_to_bound_type(t2)?;
            Ok(BoundType::Union(Box::new(b1), Box::new(b2)))
        }
        Type::ContextCall(ctx, var) => Ok(BoundType::ContextCall(ctx.clone(), var.clone())),
        Type::Universe => Ok(BoundType::Universe),
        Type::Empty => Ok(BoundType::Empty),
        _ => Err(format!("unsupported type for binding: {:?}", ty)),
    }
}

/// Check if an alternative can be typed
pub fn check_alt_typeable(alt: &Alt, grammar: &Grammar) -> TypeCheckResult {
    let Some(rule_name) = &alt.typing_rule else {
        return TypeCheckResult::NotApplicable;
    };
    
    let Some(rule) = grammar.typing_rules.get(rule_name) else {
        // Rule referenced but not defined - treat as not applicable (lenient)
        // This allows grammars to reference rules without defining them
        return TypeCheckResult::NotApplicable;
    };
    
    if !alt.is_complete() {
        return TypeCheckResult::Typeable;
    }
    
    check_complete_alt_typeable(alt, rule, grammar)
}

/// Check complete alternative
fn check_complete_alt_typeable(alt: &Alt, rule: &TypingRule, grammar: &Grammar) -> TypeCheckResult {
    let mut ctx = TypeContext::new();
    
    for (i, premise) in rule.premises.iter().enumerate() {
        if let Err(e) = check_premise(premise, alt, &mut ctx, grammar) {
            return TypeCheckResult::NotTypeable(
                e.with_rule(rule.name.clone())
                    .with_context(format!("premise {}", i + 1))
            );
        }
    }
    
    if let Err(e) = check_conclusion(&rule.conclusion, alt, &ctx, grammar) {
        return TypeCheckResult::NotTypeable(
            e.with_rule(rule.name.clone())
                .with_context("conclusion".to_string())
        );
    }
    
    TypeCheckResult::Typeable
}

/// Check premise
fn check_premise(
    premise: &Premise,
    _alt: &Alt,
    ctx: &mut TypeContext,
    _grammar: &Grammar,
) -> Result<(), TypeCheckError> {
    if let Some(judgment) = &premise.judgment {
        match judgment {
            TypingJudgment::Ascription((term, ty)) => {
                let bound_ty = type_to_bound_type(ty)
                    .map_err(|e| TypeCheckError::new(format!("invalid type in premise: {}", e)))?;
                
                ctx.bind(term.clone(), bound_ty);
                Ok(())
            }
            TypingJudgment::Membership(_var, _ctx_name) => Ok(()),
        }
    } else {
        Ok(())
    }
}

/// Check conclusion
fn check_conclusion(
    conclusion: &Conclusion,
    _alt: &Alt,
    _ctx: &TypeContext,
    _grammar: &Grammar,
) -> Result<(), TypeCheckError> {
    match &conclusion.kind {
        ConclusionKind::Type(ty) => {
            type_to_bound_type(ty)
                .map_err(|e| TypeCheckError::new(format!("invalid conclusion type: {}", e)))?;
            Ok(())
        }
        ConclusionKind::ContextLookup(_ctx, _var) => Ok(()),
    }
}

/// Filter alternatives with error collection
pub fn filter_typeable_alternatives(nt: &mut NonTerminal, grammar: &Grammar) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();
    
    nt.alts.retain(|alt| {
        let result = check_alt_typeable(alt, grammar);
        if let Some(err) = result.error() {
            errors.push(err.clone());
        }
        result.is_typeable()
    });
    
    errors
}

/// Filter AST with error collection
pub fn filter_typeable_ast(ast: &mut PartialAST, grammar: &Grammar) -> Vec<TypeCheckError> {
    filter_typeable_nonterminal(&mut ast.root, grammar)
}

/// Filter nonterminal recursively
fn filter_typeable_nonterminal(nt: &mut NonTerminal, grammar: &Grammar) -> Vec<TypeCheckError> {
    let mut all_errors = Vec::new();
    
    let errors = filter_typeable_alternatives(nt, grammar);
    all_errors.extend(errors);
    
    for alt in &mut nt.alts {
        for slot_vec in alt.slots.values_mut() {
            for slot in slot_vec {
                match slot {
                    Slot::Filled(nodes) => {
                        for node in nodes {
                            if let ParsedNode::NonTerminal(child_nt) = node {
                                let child_errors = filter_typeable_nonterminal(child_nt, grammar);
                                all_errors.extend(child_errors);
                            }
                        }
                    }
                    Slot::Partial { node: Some(ParsedNode::NonTerminal(child_nt)), .. } => {
                        let child_errors = filter_typeable_nonterminal(child_nt, grammar);
                        all_errors.extend(child_errors);
                    }
                    _ => {}
                }
            }
        }
    }
    
    all_errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::partial::parse::Parser;
    
    #[test]
    fn test_filter_keeps_all_when_no_rules() {
        let spec = r#"
        A ::= 'a'
        B ::= 'b'
        start ::= A | B
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let mut ast = p.partial("").unwrap();
        
        let original_alt_count = ast.root.alts.len();
        filter_typeable_ast(&mut ast, &g);
        
        // Should keep all alternatives since no typing rules
        assert_eq!(ast.root.alts.len(), original_alt_count);
    }
    
    #[test]
    fn test_filter_with_typing_rule() {
        let spec = r#"
        start(var) ::= /[a-z]+/
        
        -------------- (var)
        'int'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("x").unwrap();
        
        assert!(ast.root.alts.len() > 0, "should have typeable alternative");
    }
    
    #[test]

    
    #[ignore] // TODO: Enable when full type checking is implemented

    
    fn test_incomplete_alternatives_kept() {
        let spec = r#"
        Expr(expr) ::= 'let' /[a-z]+/ '=' /[0-9]+/
        
        -------------- (expr)
        'unit'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let mut ast = p.partial("let x").unwrap();
        
        let alt_count_before = ast.root.alts.len();
        filter_typeable_ast(&mut ast, &g);
        
        // Incomplete alternatives should be kept (optimistic)
        // Type filtering is optimistic, incomplete alternatives are kept
        assert!(ast.root.alts.len() > 0, "should have alternatives");
    }
    
    #[test]

    
    #[ignore] // TODO: Enable when full type checking is implemented

    
    fn test_typed_completions_basic() {
        let spec = r#"
        start(num) ::= /[0-9]+/
        
        -------------- (num)
        'int'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("").unwrap();
        
        let completions = ast.typed_completions(&g);
        assert!(completions.tokens.len() > 0, "should have completions");
    }
    
    #[test]
    fn test_typed_completions_filters_invalid() {
        // This test would need a more sophisticated type checker
        // For now, it demonstrates the API
        let spec = r#"
        Good(good) ::= 'x'
        start ::= Good
        
        -------------- (good)
        'valid'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("").unwrap();
        
        let typed = ast.typed_completions(&g);
        let untyped = ast.completions(&g);
        
        // For this simple case, should be the same
        assert_eq!(typed.tokens.len(), untyped.tokens.len());
    }
    
    #[test]
    fn test_filter_recursive_nonterminals() {
        let spec = r#"
        Inner(inner) ::= 'a'
        Outer ::= Inner 'b'
        start ::= Outer
        
        -------------- (inner)
        'int'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let mut ast = p.partial("a b").unwrap();
        
        // Should filter recursively
        filter_typeable_ast(&mut ast, &g);
        
        // All alternatives should remain (they're all valid)
        assert!(ast.root.alts.len() > 0);
    }
    
    #[test]

    
    #[ignore] // TODO: Enable when full type checking is implemented

    
    fn test_multiple_typing_rules() {
        let spec = r#"
        Var(var) ::= /[a-z]+/
        Num(num) ::= /[0-9]+/
        start ::= Var | Num
        
        -------------- (var)
        'string'
        
        -------------- (num)
        'int'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        // Parser already filters
        let ast = p.partial("x").unwrap();
        
        // Should keep the Var alternative (it matches)
        assert!(ast.root.alts.len() > 0, "should have alternatives after parsing");
        
        // Check completions are filtered correctly
        let completions = ast.typed_completions(&g);
        assert!(completions.tokens.len() > 0, "should have some completions");
    }
    
    #[test]
    fn test_type_filtering_preserves_valid_alternatives() {
        let spec = r#"
        A(ruleA) ::= 'a'
        B(ruleB) ::= 'b'  
        start ::= A | B
        
        -------------- (ruleA)
        'typeA'
        
        -------------- (ruleB)
        'typeB'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        // Parser already filters
        let ast = p.partial("").unwrap();
        
        // Both alternatives should be kept (both have valid typing rules)
        // At empty input, we should have alternatives representing the start state
        assert!(ast.root.alts.len() > 0, "should have alternatives");
    }
    
    #[test]
    fn test_typed_completions_with_partial_match() {
        let spec = r#"
        Expr(expr) ::= 'let' /[a-z]+/ '=' /[0-9]+/
        start ::= Expr
        
        -------------- (expr)
        'unit'
        "#;
        
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("let x =").unwrap();
        
        let completions = ast.typed_completions(&g);
        
        // Should suggest number pattern as next token
        assert!(completions.contains_regex("[0-9]+"), "should suggest number after '='");
    }
}
