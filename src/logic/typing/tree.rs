//! Typed AST - transforms partial AST into typed representation
//!
//! Composes on top of typing::eval which provides the core check_tree function.

use crate::debug_info;
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal, SppfForest, Terminal};
use crate::logic::typing::core::{Context, TreePath, TreeRef, TreeStatus};
use crate::logic::typing::eval::{check_node, check_tree_with_context};
use crate::logic::typing::Type;
use crate::logic::Parser;
use crate::regex::Regex as DerivativeRegex;
use std::collections::HashMap;

// ============================================================================
// Types
// ============================================================================

#[derive(Clone, Debug)]
pub enum TypedNode {
    Term {
        val: String,
        ty: Type,
        /// Remainder regex for partial terminals (what can still be typed to complete this token)
        remainder: Option<DerivativeRegex>,
        /// Extension regex for complete terminals (what may optionally follow)
        extension: Option<DerivativeRegex>,
    },
    Expr {
        name: String,
        children: Vec<TypedNode>,
        ty: Type,
        complete: bool,
        /// Index of the chosen alternative in grammar.productions[name]
        alt_index: usize,
        /// Cached RHS length for the chosen production (avoids grammar lookups in hot paths)
        rhs_len: usize,
    },
}

#[derive(Clone, Debug)]
pub struct TypedAST {
    pub roots: Vec<TypedNode>,
}

impl TypedAST {
    pub fn new(roots: Vec<TypedNode>) -> Self {
        Self { roots }
    }

    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }

    pub fn len(&self) -> usize {
        self.roots.len()
    }

    pub fn first(&self) -> Option<&TypedNode> {
        self.roots.first()
    }

    /// Reconstruct the input text from terminal values in the first root.
    pub fn text(&self) -> String {
        self.roots.first().map(|r| r.text()).unwrap_or_default()
    }

    /// Filter to complete trees (consumes self)
    pub fn completes(self) -> Result<Self, String> {
        let roots: Vec<_> = self.roots.into_iter().filter(|r| r.is_complete()).collect();
        if roots.is_empty() {
            Err("No complete trees".into())
        } else {
            Ok(Self { roots })
        }
    }

    /// Return the first complete tree
    pub fn complete(self) -> Result<TypedNode, String> {
        self.roots
            .into_iter()
            .find(|r| r.is_complete())
            .ok_or_else(|| "No complete tree found".to_string())
    }

    pub fn is_complete(&self) -> bool {
        self.roots.iter().any(|r| r.is_complete())
    }
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
            Self::Term { remainder, .. } => remainder.is_none(),
            Self::Expr { complete, .. } => *complete,
        }
    }

    /// Reconstruct the surface text from terminal values in this subtree.
    pub fn text(&self) -> String {
        match self {
            Self::Term { val, .. } => val.clone(),
            Self::Expr { children, .. } => children
                .iter()
                .map(|c| c.text())
                .collect::<Vec<_>>()
                .join(" "),
        }
    }

    /// Build typed node from non-terminal using pre-computed type cache
    pub fn from_nt(
        nt: &NonTerminal,
        path: &TreePath,
        type_cache: &HashMap<TreePath, Type>,
    ) -> Self {
        let ty = type_cache.get(path).cloned().unwrap_or(Type::Any);
        let mut children = Vec::new();

        for (i, child) in nt.children.iter().enumerate() {
            let mut child_path = path.clone();
            child_path.push(i);

            match child {
                Node::Terminal(Terminal::Complete {
                    value, extension, ..
                }) => {
                    let child_ty = type_cache.get(&child_path).cloned().unwrap_or(Type::Any);
                    children.push(Self::Term {
                        val: value.clone(),
                        ty: child_ty,
                        remainder: None,
                        extension: extension.clone(),
                    });
                }
                Node::Terminal(Terminal::Partial {
                    value, remainder, ..
                }) => {
                    let child_ty = type_cache.get(&child_path).cloned().unwrap_or(Type::Any);
                    children.push(Self::Term {
                        val: value.clone(),
                        ty: child_ty,
                        remainder: remainder.clone(),
                        extension: None,
                    });
                }
                Node::NonTerminal(child_nt) => {
                    children.push(Self::from_nt(child_nt, &child_path, type_cache));
                }
            }
        }

        Self::Expr {
            name: nt.name.clone(),
            children,
            ty,
            complete: nt.is_complete(),
            alt_index: nt.alternative_index,
            rhs_len: nt.production.rhs.len(),
        }
    }
}

// ============================================================================
// TypedAST
// ============================================================================
// SppfForest → TypedAST
// ============================================================================

impl SppfForest {
    /// Type-check and transform to TypedAST (all possible well-typed parses)
    pub fn typed(&self, g: &Grammar) -> Result<TypedAST, String> {
        self.typed_ctx(g, &Context::new())
    }

    pub fn typed_ctx(&self, g: &Grammar, ctx: &Context) -> Result<TypedAST, String> {
        let mut roots: Vec<TypedNode> = Vec::new();

        for root_id in self.root_ids() {
            debug_info!(
                "typing",
                "Typing SPPF root {} with grammar {}",
                root_id,
                g.name
            );
            self.for_each_materialized_root(*root_id, |r| {
                let mut type_cache = HashMap::new();
                let tref = TreeRef::new(&r, vec![]);
                let status = check_node(&tref, g, ctx, 0, &mut type_cache);

                if !matches!(status, TreeStatus::Malformed | TreeStatus::TooDeep) {
                    roots.push(TypedNode::from_nt(&r, &vec![], &type_cache));
                }
                true
            });
        }

        if roots.is_empty() {
            Err("No well-typed trees".into())
        } else {
            Ok(TypedAST { roots })
        }
    }

    /// typed().completes() - composition
    pub fn typed_complete(&self, g: &Grammar) -> Result<TypedAST, String> {
        let ast = self.typed(g)?;
        ast.completes()
    }

    pub fn typed_complete_ctx(&self, g: &Grammar, ctx: &Context) -> Result<TypedAST, String> {
        self.typed_ctx(g, ctx)?.completes()
    }

    /// Simple predicate: any well-typed tree exists?
    pub fn has_well_typed(&self, g: &Grammar) -> bool {
        self.has_well_typed_ctx(g, &Context::new())
    }

    pub fn has_well_typed_ctx(&self, g: &Grammar, ctx: &Context) -> bool {
        for root_id in self.root_ids() {
            let mut found = false;
            self.for_each_materialized_root(*root_id, |r| {
                if check_tree_with_context(&r, g, ctx).is_ok() {
                    found = true;
                    return false;
                }
                true
            });
            if found {
                return true;
            }
        }
        false
    }

    pub fn has_valid_or_partial_ctx(&self, g: &Grammar, ctx: &Context) -> bool {
        for root_id in self.root_ids() {
            let mut found = false;
            self.for_each_materialized_root(*root_id, |r| {
                let status = check_tree_with_context(&r, g, ctx);
                if matches!(status, TreeStatus::Valid(_) | TreeStatus::Partial(_)) {
                    found = true;
                    return false;
                }
                true
            });
            if found {
                return true;
            }
        }
        false
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
            Some(TypedNode::from_nt(self, &vec![], &type_cache))
        }
    }
}

impl Parser {
    pub fn partial_typed(&mut self, input: &str) -> Result<TypedAST, String> {
        let partial_ast = self
            .partial(input)
            .into_result()
            .map_err(|e| e.to_string())?;
        partial_ast.typed(&self.grammar)
    }

    pub fn partial_typed_ctx(&mut self, input: &str, ctx: &Context) -> Result<TypedAST, String> {
        let partial_ast = self
            .partial(input)
            .into_result()
            .map_err(|e| e.to_string())?;
        partial_ast.typed_ctx(&self.grammar, ctx)
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
        for (i, root) in self.roots.iter().enumerate() {
            writeln!(f, "\nTree {}:", i)?;
            write!(f, "{}", root)?;
        }
        Ok(())
    }
}
