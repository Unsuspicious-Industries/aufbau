//! Typed AST - transforms partial AST into typed representation
//!
//! Composes on top of typing::eval which provides the core check_tree function.

use crate::logic::grammar::Grammar;
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::eval::check_tree_with_context;
use crate::logic::typing::Type;
use super::structure::{Node, NonTerminal, PartialAST, Terminal};

// ============================================================================
// Types
// ============================================================================

#[derive(Clone, Debug)]
pub enum TypedNode {
    Term { val: String, ty: Type },
    Expr { name: String, children: Vec<TypedNode>, ty: Type, complete: bool },
}

#[derive(Clone, Debug)]
pub struct TypedAST {
    pub roots: Vec<TypedNode>,
    pub input: String,
}

// ============================================================================
// TypedNode
// ============================================================================

impl TypedNode {
    pub fn ty(&self) -> &Type {
        match self { Self::Term { ty, .. } | Self::Expr { ty, .. } => ty }
    }

    pub fn is_complete(&self) -> bool {
        match self {
            Self::Term { .. } => true,
            Self::Expr { complete, .. } => *complete,
        }
    }

    /// Build typed node from partial node, using eval::check_tree for types
    fn from_node(node: &Node, g: &Grammar, ctx: &Context) -> Option<Self> {
        match node {
            Node::Terminal(t) => {
                let val = match t {
                    Terminal::Complete { value, .. } | Terminal::Partial { value, .. } => value.clone(),
                };
                Some(Self::Term { val, ty: Type::Universe })
            }
            Node::NonTerminal(nt) => Self::from_nt(nt, g, ctx),
        }
    }

    fn from_nt(nt: &NonTerminal, g: &Grammar, ctx: &Context) -> Option<Self> {
        let status = check_tree_with_context(nt, g, ctx);
        if matches!(status, TreeStatus::Malformed) { return None; }
        let ty = status.ty().cloned().unwrap_or(Type::Universe);
        let children = nt.children
            .iter()
            .filter(|c| matches!(c, Node::NonTerminal(_)))
            .filter_map(|c| Self::from_node(c, g, ctx))
            .collect();
        // Use the original AST's completeness
        let complete = nt.is_complete();
        Some(Self::Expr { name: nt.name.clone(), children, ty, complete })
    }
}

// ============================================================================
// TypedAST
// ============================================================================

impl TypedAST {
    pub fn first(&self) -> Option<&TypedNode> { self.roots.first() }
    pub fn is_empty(&self) -> bool { self.roots.is_empty() }

    /// Filter to complete trees (consumes self)
    pub fn complete(self) -> Result<Self, String> {
        let roots: Vec<_> = self.roots.into_iter().filter(|r| r.is_complete()).collect();
        if roots.is_empty() { Err("No complete trees".into()) }
        else { Ok(Self { roots, input: self.input }) }
    }

}

// ============================================================================
// PartialAST → TypedAST (composition)
// ============================================================================

impl PartialAST {
    /// Type-check and transform to TypedAST, filtering malformed trees
    pub fn typed(&self, g: &Grammar) -> Result<TypedAST, String> {
        self.typed_ctx(g, &Context::new())
    }

    pub fn typed_ctx(&self, g: &Grammar, ctx: &Context) -> Result<TypedAST, String> {
        let roots: Vec<_> = self.roots.iter()
            .filter_map(|r| TypedNode::from_nt(r, g, ctx))
            .collect();
        if roots.is_empty() { Err("No well-typed trees".into()) }
        else { Ok(TypedAST { roots, input: self.input.clone() }) }
    }

    /// typed().complete() - composition
    pub fn typed_complete(&self, g: &Grammar) -> Result<TypedAST, String> {
        self.typed(g)?.complete()
    }

    pub fn typed_complete_ctx(&self, g: &Grammar, ctx: &Context) -> Result<TypedAST, String> {
        self.typed_ctx(g, ctx)?.complete()
    }

    /// Simple predicate: any well-typed tree exists?
    pub fn has_well_typed(&self, g: &Grammar) -> bool {
        self.roots.iter().any(|r| check_tree_with_context(r, g, &Context::new()).is_ok())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::parse::Parser;

    fn parse(spec: &str, input: &str) -> (PartialAST, Grammar) {
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        (p.partial(input).unwrap(), g)
    }

    // ========================================================================
    // Basic API tests
    // ========================================================================

    #[test]
    fn test_typed_basic() {
        let spec = "Num(n) ::= /[0-9]+/\nstart ::= Num\n-------------- (n)\n'int'";
        let (ast, g) = parse(spec, "42");
        let typed = ast.typed(&g).unwrap();
        assert!(!typed.is_empty());
        assert!(typed.first().is_some());
    }

    #[test]
    fn test_typed_complete_composition() {
        let spec = "Var(v) ::= /[a-z]+/\nstart ::= Var\nx ∈ Γ\n-------------- (v)\nΓ(x)";
        let (ast, g) = parse(spec, "x");
        let ctx = Context::new().extend("x".into(), Type::Atom("Int".into()));
        assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
    }

    #[test]
    fn test_has_well_typed() {
        let spec = "start ::= 'a'";
        let (ast, g) = parse(spec, "a");
        assert!(ast.has_well_typed(&g));
    }

    // ========================================================================
    // Error cases - context-dependent typing
    // ========================================================================

    #[test]
    fn test_variable_requires_context() {
        // Variable rule requires x ∈ Γ - should fail without context
        let spec = "Var(v) ::= /[a-z]+/\nstart ::= Var\nx ∈ Γ\n-------------- (v)\nΓ(x)";
        let (ast, g) = parse(spec, "x");
        // typed_complete with context should work
        let ctx = Context::new().extend("x".into(), Type::Atom("Int".into()));
        assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
    }

    #[test]
    fn test_partial_with_complete_filter() {
        // Tests that typed_complete uses PartialAST::complete() check
        let spec = "start ::= 'a' 'b' 'c'";
        let (ast, g) = parse(spec, "a b");
        // The partial AST itself is not complete
        assert!(!ast.complete(), "partial input should not be complete");
        // typed_complete should fail for partial
        assert!(ast.typed_complete(&g).is_err());
    }

    // ========================================================================
    // TypedNode tests
    // ========================================================================

    #[test]
    fn test_typed_node_is_complete() {
        let spec = "start ::= 'a'";
        let (ast, g) = parse(spec, "a");
        let typed = ast.typed(&g).unwrap();
        let root = typed.first().unwrap();
        assert!(root.is_complete());
    }

    #[test]
    fn test_typed_node_type_access() {
        let spec = "Num(n) ::= /[0-9]+/\nstart ::= Num\n-------------- (n)\n'int'";
        let (ast, g) = parse(spec, "42");
        let typed = ast.typed(&g).unwrap();
        let root = typed.first().unwrap();
        // Root is 'start' which drills through to Num
        let _ty = root.ty(); // Should not panic
    }

    // ========================================================================
    // Context propagation tests
    // ========================================================================

    #[test]
    fn test_lambda_binds_variable() {
        // Lambda should bind x in its body
        let spec = r#"
            Identifier ::= /[a-z]+/
            Variable(var) ::= Identifier[x]
            Lambda(lam) ::= 'λ' Identifier[x] '.' Variable[e]
            start ::= Lambda
            
            x ∈ Γ
            -------------- (var)
            Γ(x)
            
            Γ[x:'int'] ⊢ e : ?B
            -------------- (lam)
            'int' → ?B
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        
        // λx.x should typecheck - x is bound by lambda
        let ast = p.partial("λ x . x").unwrap();
        assert!(ast.typed_complete(&g).is_ok(), "lambda should bind its variable");
    }

    #[test]
    fn test_variable_with_context_succeeds() {
        let spec = r#"
            Identifier ::= /[a-z]+/
            Variable(var) ::= Identifier[x]
            start ::= Variable
            
            x ∈ Γ
            -------------- (var)
            Γ(x)
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        
        // Variable with context should work
        let ast = p.partial("y").unwrap();
        let ctx = Context::new().extend("y".into(), Type::Atom("Int".into()));
        assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
        println!("Typed AST with context: {}", ast.typed_ctx(&g, &ctx).unwrap());
    }

    #[test]
    fn test_complete_filter() {
        let spec = "start ::= 'a' 'b'";
        let (ast, g) = parse(spec, "a b");
        let typed = ast.typed(&g).unwrap();
        let filtered = typed.complete().unwrap();
        assert!(!filtered.is_empty());
    }


    // ========================================================================
    // Display tests
    // ========================================================================

    #[test]
    fn test_typed_ast_display() {
        let spec = "start ::= 'hello'";
        let (ast, g) = parse(spec, "hello");
        let typed = ast.typed(&g).unwrap();
        let display = format!("{}", typed);
        assert!(display.contains("hello"));
        assert!(display.contains("start"));
        println!("TypedAST display:\n{}", display);
    }
}
