//! Typed AST - transforms partial AST into typed representation
//!
//! Composes on top of typing::eval which provides the core check_tree function.

use crate::logic::Parser;
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal, PartialAST, Terminal};
use crate::logic::typing::Type;
use crate::logic::typing::core::{Context, TreePath, TreeRef, TreeStatus};
use crate::logic::typing::eval::{check_node, check_tree_with_context};
use std::collections::HashMap;

// ============================================================================
// Types
// ============================================================================

#[derive(Clone, Debug)]
pub enum TypedNode {
    Term {
        val: String,
        ty: Type,
    },
    Expr {
        name: String,
        children: Vec<TypedNode>,
        ty: Type,
        complete: bool,
    },
}

#[derive(Clone, Debug)]
pub struct TypedAST {
    pub roots: Vec<TypedNode>,
    pub input: String,
}

// ============================================================================
// TypedNode - Efficient construction using type cache
// ============================================================================

impl TypedNode {
    pub fn ty(&self) -> &Type {
        match self {
            Self::Term { ty, .. } | Self::Expr { ty, .. } => ty,
        }
    }

    pub fn is_complete(&self) -> bool {
        match self {
            Self::Term { .. } => true,
            Self::Expr { complete, .. } => *complete,
        }
    }

    /// Build typed node from partial node using pre-computed type cache
    fn from_node_with_cache(
        root: &NonTerminal,
        path: &TreePath,
        node: &Node,
        type_cache: &HashMap<TreePath, Type>,
    ) -> Option<Self> {
        match node {
            Node::Terminal(t) => {
                let val = match t {
                    Terminal::Complete { value, .. } | Terminal::Partial { value, .. } => {
                        value.clone()
                    }
                };
                // For terminals, use cached type if available, otherwise Any
                let ty = type_cache.get(path).cloned().unwrap_or(Type::Any);
                Some(Self::Term { val, ty })
            }
            Node::NonTerminal(nt) => Self::from_nt_with_cache(root, path, nt, type_cache),
        }
    }

    fn from_nt_with_cache(
        root: &NonTerminal,
        path: &TreePath,
        nt: &NonTerminal,
        type_cache: &HashMap<TreePath, Type>,
    ) -> Option<Self> {
        // Get type from cache - if not found, use Any as fallback
        let ty = type_cache.get(path).cloned().unwrap_or(Type::Any);

        // Build children by traversing non-terminal children and looking up their types
        let mut children = Vec::new();
        for (i, child) in nt.children.iter().enumerate() {
            if matches!(child, Node::NonTerminal(_)) {
                let mut child_path = path.clone();
                child_path.push(i);

                if let Some(child_node) =
                    Self::from_node_with_cache(root, &child_path, child, type_cache)
                {
                    children.push(child_node);
                }
            }
        }

        // Use the original AST's completeness
        let complete = nt.is_complete();
        Some(Self::Expr {
            name: nt.name.clone(),
            children,
            ty,
            complete,
        })
    }
}

// ============================================================================
// TypedAST
// ============================================================================

impl TypedAST {
    pub fn first(&self) -> Option<&TypedNode> {
        self.roots.first()
    }
    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }

    /// Filter to complete trees (consumes self)
    pub fn complete(self) -> Result<Self, String> {
        let roots: Vec<_> = self.roots.into_iter().filter(|r| r.is_complete()).collect();
        if roots.is_empty() {
            Err("No complete trees".into())
        } else {
            Ok(Self {
                roots,
                input: self.input,
            })
        }
    }
}

// ============================================================================
// PartialAST → TypedAST (composition) - Efficient version
// ============================================================================

impl PartialAST {
    /// Type-check and transform to TypedAST using efficient type cache
    pub fn typed(&self, g: &Grammar) -> Result<TypedAST, String> {
        self.typed_ctx(g, &Context::new())
    }

    pub fn typed_ctx(&self, g: &Grammar, ctx: &Context) -> Result<TypedAST, String> {
        let roots: Vec<_> = self
            .roots
            .iter()
            .filter_map(|r| {
                // First check the tree and build type cache
                let mut type_cache = HashMap::new();
                let tref = TreeRef::new(r, vec![]);
                let status = check_node(&tref, g, ctx, 0, &mut type_cache);

                // Only proceed if tree is well-typed
                if matches!(status, TreeStatus::Malformed | TreeStatus::TooDeep) {
                    None
                } else {
                    // Build typed node using the cache

                    TypedNode::from_nt_with_cache(r, &vec![], r, &type_cache)
                }
            })
            .collect();

        if roots.is_empty() {
            Err("No well-typed trees".into())
        } else {
            Ok(TypedAST {
                roots,
                input: self.input.clone(),
            })
        }
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
        self.roots
            .iter()
            .any(|r| check_tree_with_context(r, g, &Context::new()).is_ok())
    }

    // filter to an AST with only well-typed trees
    pub fn filter_typed(&self, g: &Grammar) -> Result<PartialAST, String> {
        let roots: Vec<_> = self
            .roots
            .iter()
            .filter(|r| check_tree_with_context(r, g, &Context::new()).is_ok())
            .cloned()
            .collect();
        match roots.is_empty() {
            true => Err("No well-typed trees".into()),
            false => Ok(PartialAST {
                roots,
                input: self.input.clone(),
            }),
        }
    }
}

impl NonTerminal {
    /// Efficient typed tree construction using type cache
    pub fn typed(&self, g: &Grammar) -> Option<TypedNode> {
        let mut type_cache = HashMap::new();
        let tref = TreeRef::new(self, vec![]);
        let status = check_node(&tref, g, &Context::new(), 0, &mut type_cache);

        if matches!(status, TreeStatus::Malformed | TreeStatus::TooDeep) {
            None
        } else {
            TypedNode::from_nt_with_cache(self, &vec![], self, &type_cache)
        }
    }
}

impl Parser {
    pub fn partial_typed(&mut self, input: &str) -> Result<PartialAST, String> {
        let partial_ast = self.partial(input)?;
        partial_ast.filter_typed(&self.grammar)
    }
}

// ============================================================================
// TypedAST Display (IDE-style type annotations)
// ============================================================================

use std::fmt;
use std::fmt::Display;

impl TypedNode {
    fn fmt_tree(&self, f: &mut fmt::Formatter<'_>, prefix: &str, is_last: bool) -> fmt::Result {
        use crate::logic::typing::Type;
        let branch = if is_last { "└─ " } else { "├─ " };
        let ty_str = match self.ty() {
            Type::Any => String::new(),
            t => format!(" : {}", t),
        };
        match self {
            Self::Term { val, .. } => writeln!(f, "{}{}{}{}", prefix, branch, val, ty_str),
            Self::Expr { name, children, .. } => {
                writeln!(f, "{}{}{}{}", prefix, branch, name, ty_str)?;
                let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "│  " });
                for (i, child) in children.iter().enumerate() {
                    child.fmt_tree(f, &child_prefix, i == children.len() - 1)?;
                }
                Ok(())
            }
        }
    }
}

impl Display for TypedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_tree(f, "", true)
    }
}

impl Display for TypedAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Input: \"{}\"", self.input)?;
        for (i, root) in self.roots.iter().enumerate() {
            writeln!(f, "\nTree {}:", i)?;
            write!(f, "{}", root)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::parse::Parser;
    use crate::set_debug_level;

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
        let spec = "
Num(n) ::= /[0-9]+/
start ::= Num

-------------- (n)
'int'
";
        let (ast, g) = parse(spec, "42");
        let typed = ast.typed(&g).unwrap();
        assert!(!typed.is_empty());
        assert!(typed.first().is_some());
        println!("typed: {}", typed);
    }

    #[test]
    fn test_typed_complete_composition() {
        let spec = "
            Var(v) ::= /[a-z]+/[x]
            start ::= Var

            x ∈ Γ
            -------------- (v)
            Γ(x)
            ";
        let (ast, g) = parse(spec, "x");
        let ctx = Context::new().extend("x".into(), Type::Raw("Int".into())).unwrap();
        assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
        let typed = ast.typed_complete_ctx(&g, &ctx).unwrap();
        println!("typed: {}", typed);
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
        set_debug_level(crate::DebugLevel::Trace);
        // Variable rule requires x ∈ Γ - should fail without context
        let spec = "
            Var(v) ::= /[a-z]+/[e]
            start ::= Var

            e ∈ Γ
            -------------- (v)
            Γ(e)";
        let (ast, g) = parse(spec, "x");
        // typed_complete with context should work
        let ctx = Context::new().extend("x".into(), Type::Atom("Int".into())).unwrap();
        assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
        let typed = ast.typed_complete_ctx(&g, &ctx).unwrap();
        println!("typed: {}", typed);
    }

    #[test]
    fn test_partial_with_complete_filter() {
        // Tests that typed_complete uses PartialAST::complete() check
        let spec = "start ::= 'a' 'b' 'c'";
        let (ast, g) = parse(spec, "a b");
        // The partial AST itself is not complete
        assert!(!ast.is_complete(), "partial input should not be complete");
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
        assert!(
            ast.typed_complete(&g).is_ok(),
            "lambda should bind its variable"
        );
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
        let ctx = Context::new().extend("y".into(), Type::Atom("Int".into())).unwrap();
        assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
        println!(
            "Typed AST with context: {}",
            ast.typed_ctx(&g, &ctx).unwrap()
        );
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
