//! Typing Rules — data structures, parsing, and meta compilation.
//!
//! # Architecture (matching §3 of the draft)
//!
//! 1. **Rule text** is parsed into `TypingRule` with `TypeExpr` in premises
//!    and conclusions.  Metas (`?A`) appear as `TypeExpr::Meta`.
//! 2. **Compilation** (`CompilationPass`) eliminates Metas via:
//!    - *Arrow decomposition*: `b : ?A → ?B` is desugared into
//!      `typeof(b) = ?fresh1 → ?fresh2` with Meta bindings.
//!    - *Meta unification*: same-named Metas at different positions
//!      become equality constraints on `typeof` projections.
//! 3. **CompiledRule**: the result — no `TypeExpr::Meta` variants remain.
//!    All checking is by equality/inclusion/ascription on `TypeExpr`
//!    whose runtime expressions (`TypeOf`, `ContextExt`) are resolved
//!    against obligations and context.
//!
//! # Pattern matching (§3 draft)
//!
//!   $b : ?A → ?B$  means that `typeof(b)` must be an arrow type,
//!   with domain projected to the position of `?A` and codomain
//!   projected to the position of `?B`.

use crate::domains::typing::ContextTransition;
use super::{Type, TypeExpr};
use regex::Regex;
use std::collections::HashMap;
use std::fmt;

// =============================================================================
// DATA STRUCTURES
// =============================================================================

/// Operations that can appear in premises (e.g., τ₁ = τ₂, τ₁ ⊆ τ₂)
#[derive(Debug, Clone, PartialEq)]
pub enum TypeOperation {
    Equality,
    Inclusion,
}

/// Term representation (binding name in the grammar)
pub type Term = String;

/// A type ascription: term : type_expr
pub type TypeAscription = (Term, TypeExpr);

/// Typing context with optional extensions: Γ or Γ[x:τ]
#[derive(Debug, Clone)]
pub struct TypeSetting {
    pub name: String,
    pub extensions: Vec<(String, TypeExpr)>,
    pub no_propagate: bool,
}

/// A judgment that can appear in premises
#[derive(Debug, Clone)]
pub enum TypingJudgment {
    /// Type ascription: e : τ  (τ is a TypeExpr)
    Ascription(TypeAscription),
    /// Context membership: x ∈ Γ
    Membership(String, String),
    /// Type operation: τ₁ op τ₂  (both are TypeExpr)
    Operation {
        left: TypeExpr,
        op: TypeOperation,
        right: TypeExpr,
    },
    /// Generated equality constraint: τ₁ = τ₂  (from meta compilation)
    Equality { left: TypeExpr, right: TypeExpr },
}

/// A premise in a typing rule
#[derive(Debug, Clone)]
pub struct Premise {
    pub setting: Option<TypeSetting>,
    pub judgment: Option<TypingJudgment>,
}

/// Verdict after premise evaluation.
/// Lattice structure: Contradiction < Unknown < Satisfied
pub enum PremiseStatus {
    Satisfied,
    Unknown,
    Contradiction,
}

/// Context transform specification in a conclusion
#[derive(Debug, Clone, Default)]
pub struct ConclusionContext {
    pub input: String,
    pub output: Option<TypeSetting>,
}

impl ConclusionContext {
    pub fn is_empty(&self) -> bool {
        self.input.is_empty() && self.output.is_none()
    }
}

/// The conclusion of a typing rule
#[derive(Debug, Clone)]
pub struct Conclusion {
    pub context: ConclusionContext,
    /// The result type expression (elaborated to evidence at eval time)
    pub kind: TypeExpr,
}

/// A complete typing rule with premises and conclusion.
/// Metas (`?A`) may still appear — they are eliminated by `CompilationPass`.
#[derive(Debug, Clone)]
pub struct TypingRule {
    pub name: String,
    pub premises: Vec<Premise>,
    pub conclusion: Conclusion,
}

/// Rule evaluation result
pub enum RuleResult {
    Success((Type, Option<ContextTransition>)),
    Partial(Type),
    Contradiction,
}

// =============================================================================
// CompiledRule — post-compilation form with no Metas
// =============================================================================

/// A compiled rule ready for evaluation.  Fresh internal Metas (`_0`, `_1`, …)
/// may remain; they are resolved at evaluation time by the meta-substitution
/// map in `apply_rule` via `bind_ascription_metas` and `bind_equality_metas`.
#[derive(Debug, Clone)]
pub struct CompiledRule {
    pub name: String,
    pub premises: Vec<Premise>,
    pub conclusion: Conclusion,
}

impl CompiledRule {
    pub fn new(name: String, premises: Vec<Premise>, conclusion: Conclusion) -> Self {
        Self { name, premises, conclusion }
    }

    fn assert_no_metas_in_judgment(j: &TypingJudgment, rule_name: &str) {
        match j {
            TypingJudgment::Ascription((_, ty)) => {
                debug_assert!(!ty.has_metas(), "ascription in {} has Metas", rule_name);
            }
            TypingJudgment::Operation { left, right, .. } => {
                debug_assert!(!left.has_metas(), "operation left in {} has Metas", rule_name);
                debug_assert!(!right.has_metas(), "operation right in {} has Metas", rule_name);
            }
            TypingJudgment::Equality { left, right } => {
                debug_assert!(!left.has_metas(), "equality left in {} has Metas", rule_name);
                debug_assert!(!right.has_metas(), "equality right in {} has Metas", rule_name);
            }
            TypingJudgment::Membership(_, _) => {}
        }
    }

    /// Check post-compilation invariants.
    pub fn validate(&self) -> Result<(), String> {
        if self.conclusion.kind.has_metas() {
            return Err(format!("conclusion of {} has Metas", self.name));
        }
        for (i, p) in self.premises.iter().enumerate() {
            if let Some(j) = &p.judgment {
                Self::check_judgment_no_metas(j, &self.name, i)?;
            }
            if let Some(s) = &p.setting {
                for (name, ty) in &s.extensions {
                    if ty.has_metas() {
                        return Err(format!(
                            "setting {}.{} of {} has Metas",
                            name, i, self.name
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn check_judgment_no_metas(j: &TypingJudgment, rule: &str, i: usize) -> Result<(), String> {
        match j {
            TypingJudgment::Ascription((_, ty)) if ty.has_metas() => {
                Err(format!("ascription premise {} of {} has Metas", i, rule))
            }
            TypingJudgment::Operation { left, right, .. } => {
                if left.has_metas() {
                    return Err(format!("operation left premise {} of {} has Metas", i, rule));
                }
                if right.has_metas() {
                    return Err(format!("operation right premise {} of {} has Metas", i, rule));
                }
                Ok(())
            }
            TypingJudgment::Equality { left, right } => {
                if left.has_metas() {
                    return Err(format!("equality left premise {} of {} has Metas", i, rule));
                }
                if right.has_metas() {
                    return Err(format!("equality right premise {} of {} has Metas", i, rule));
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

// =============================================================================
// CompilationPass — §3 Meta compilation
// =============================================================================

/// Compiles a `TypingRule` (with `TypeExpr::Meta`) into a `CompiledRule`
/// (fresh internal metas retained; resolved at evaluation time).
///
/// Pipeline (draft §3 “Meta compilation pipeline”, sec:meta-compilation)
///
/// **Phase 1 — Fresh meta generation.**
/// Each user-named `?A` → fresh `_k`.  Top-level ascriptions `b : ?A`
/// emit `typeof(b) = _k`.  Nested metas (inside arrows) skip the typeof
/// constraint — decomposition is deferred to evaluation.
///
/// **Phase 2 — Shared-meta equality.**
/// Same-named metas at distinct positions link their fresh metas:
/// `_i = _j`.
///
/// After compilation the rule holds only fresh internal metas; these are
/// resolved at evaluation time by `bind_ascription_metas` (domain.rs)
/// and `bind_equality_metas` (domain.rs) via a local meta-substitution map.
pub struct CompilationPass;

/// Tracks how a Meta name maps to a specific `TypeOf` + projection position.
#[derive(Debug, Clone, PartialEq, Eq)]
enum MetaResolution {
    /// The meta maps to `typeof(binding)`.
    TypeOf(String),
    /// The meta maps to a fresh internal variable (unresolved).
    Fresh(usize),
}

impl CompilationPass {
    /// Compile a `TypingRule` into a `CompiledRule`.
    pub fn compile(rule: &TypingRule) -> Result<CompiledRule, String> {
        if !Self::has_metas(rule) {
            return Ok(CompiledRule {
                name: rule.name.clone(),
                premises: rule.premises.clone(),
                conclusion: rule.conclusion.clone(),
            });
        }

        let compiler = MetaCompiler::new(rule);
        let (premises, conclusion) = compiler.compile()?;

        Ok(CompiledRule::new(rule.name.clone(), premises, conclusion))
    }

    pub fn has_metas(rule: &TypingRule) -> bool {
        if rule.conclusion.kind.has_metas() {
            return true;
        }
        for p in &rule.premises {
            if p.has_metas() {
                return true;
            }
        }
        false
    }
}

/// Internal state for meta compilation.
struct MetaCompiler<'a> {
    rule: &'a TypingRule,
    /// Meta name → (typeof binding, optional component index for arrows).
    /// component: None = whole type, Some(0) = domain, Some(1) = codomain.
    meta_bindings: HashMap<String, TypeExpr>,
    /// Generated equality constraints (from shared metas).
    equalities: Vec<(TypeExpr, TypeExpr)>,
    /// Counter for fresh internal variables.
    fresh_counter: usize,
}

impl<'a> MetaCompiler<'a> {
    fn new(rule: &'a TypingRule) -> Self {
        Self {
            rule,
            meta_bindings: HashMap::new(),
            equalities: Vec::new(),
            fresh_counter: 0,
        }
    }

    fn fresh(&mut self) -> usize {
        let n = self.fresh_counter;
        self.fresh_counter += 1;
        n
    }

    fn compile(mut self) -> Result<(Vec<Premise>, Conclusion), String> {
        let mut premises: Vec<Premise> = Vec::new();

        for premise in &self.rule.premises {
            let compiled_premise = self.compile_premise(premise)?;
            premises.push(compiled_premise);
        }

        // Add equality constraints from shared metas
        for (left, right) in &self.equalities {
            premises.push(Premise {
                setting: None,
                judgment: Some(TypingJudgment::Equality {
                    left: left.clone(),
                    right: right.clone(),
                }),
            });
        }

        let conclusion = self.compile_conclusion(&self.rule.conclusion)?;

        Ok((premises, conclusion))
    }

    fn compile_premise(&mut self, premise: &Premise) -> Result<Premise, String> {
        let setting = if let Some(setting) = &premise.setting {
            let extensions: Vec<(String, TypeExpr)> = setting
                .extensions
                .iter()
                .map(|(name, ty)| {
                    let compiled = self.compile_expression(ty, name);
                    (name.clone(), compiled)
                })
                .collect();
            Some(TypeSetting {
                name: setting.name.clone(),
                extensions,
                no_propagate: setting.no_propagate,
            })
        } else {
            None
        };

        let judgment = if let Some(judgment) = &premise.judgment {
            Some(self.compile_judgment(judgment)?)
        } else {
            None
        };

        Ok(Premise { setting, judgment })
    }

    fn compile_judgment(&mut self, judgment: &TypingJudgment) -> Result<TypingJudgment, String> {
        match judgment {
            TypingJudgment::Ascription((term, ty)) => {
                let compiled_ty = self.compile_expression(ty, term);
                Ok(TypingJudgment::Ascription((term.clone(), compiled_ty)))
            }
            TypingJudgment::Membership(var, ctx) => {
                Ok(TypingJudgment::Membership(var.clone(), ctx.clone()))
            }
            TypingJudgment::Operation { left, op, right } => {
                let l = self.compile_expression(left, "");
                let r = self.compile_expression(right, "");
                Ok(TypingJudgment::Operation { left: l, op: op.clone(), right: r })
            }
            TypingJudgment::Equality { left, right } => {
                let l = self.compile_expression(left, "");
                let r = self.compile_expression(right, "");
                Ok(TypingJudgment::Equality { left: l, right: r })
            }
        }
    }

    /// Replace Metas with fresh internal metas. When `binding` is non-empty
    /// (top-level ascription), generate `typeof(binding) = fresh` constraints.
    /// For nested Metas (inside arrows), only register the fresh meta mapping.
    fn compile_expression(&mut self, ty: &TypeExpr, binding: &str) -> TypeExpr {
        match ty {
            TypeExpr::Arrow(domain, codomain) => {
                TypeExpr::Arrow(
                    Box::new(self.compile_expression(domain, "")),
                    Box::new(self.compile_expression(codomain, "")),
                )
            }
            TypeExpr::Meta(name) => {
                let fresh = self.fresh_meta_name();
                let fresh_ty = TypeExpr::Meta(fresh);
                if let Some(existing) = self.meta_bindings.get(name) {
                    self.equalities.push((existing.clone(), fresh_ty.clone()));
                } else {
                    self.meta_bindings.insert(name.to_string(), fresh_ty.clone());
                }
                // Only for top-level ascription metas: typeof(binding) = fresh
                if !binding.is_empty() {
                    self.equalities.push((
                        TypeExpr::TypeOf(binding.to_string()),
                        fresh_ty.clone(),
                    ));
                }
                fresh_ty
            }
            other => other.clone(),
        }
    }

    fn compile_conclusion(&mut self, conclusion: &Conclusion) -> Result<Conclusion, String> {
        let output = if let Some(out) = &conclusion.context.output {
            let exts: Vec<(String, TypeExpr)> = out.extensions.iter()
                .map(|(n, ty)| (n.clone(), self.compile_expression(ty, n)))
                .collect();
            Some(TypeSetting {
                name: out.name.clone(), extensions: exts, no_propagate: out.no_propagate,
            })
        } else { None };
        let ctx = ConclusionContext { input: conclusion.context.input.clone(), output };
        let kind = self.compile_expression(&conclusion.kind, "");
        Ok(Conclusion { context: ctx, kind })
    }

    fn fresh_meta_name(&mut self) -> String {
        let n = self.fresh_counter;
        self.fresh_counter += 1;
        format!("_{}", n)
    }
}

// =============================================================================
// HasBindings impl
// =============================================================================

impl crate::semantics::domain::HasBindings for TypingRule {
    fn referenced_bindings(&self) -> Box<dyn Iterator<Item = &str> + '_> {
        Box::new(self.used_bindings().into_iter())
    }
}

// =============================================================================
// HasMetas helpers
// =============================================================================

impl Premise {
    fn has_metas(&self) -> bool {
        if let Some(s) = &self.setting {
            if s.extensions.iter().any(|(_, ty)| ty.has_metas()) {
                return true;
            }
        }
        if let Some(j) = &self.judgment {
            return j.has_metas();
        }
        false
    }
}

impl TypingJudgment {
    fn has_metas(&self) -> bool {
        match self {
            TypingJudgment::Ascription((_, ty)) => ty.has_metas(),
            TypingJudgment::Operation { left, right, .. } => {
                left.has_metas() || right.has_metas()
            }
            TypingJudgment::Equality { left, right } => {
                left.has_metas() || right.has_metas()
            }
            TypingJudgment::Membership(_, _) => false,
        }
    }
}

// =============================================================================
// RULE ANALYSIS
// =============================================================================

impl TypingRule {
    /// Get the set of binding names referenced by this rule.
    pub fn used_bindings(&self) -> std::collections::HashSet<&str> {
        let mut bindings = std::collections::HashSet::new();

        for premise in &self.premises {
            if let Some(setting) = &premise.setting {
                for (var, ty) in &setting.extensions {
                    bindings.insert(var.as_str());
                    Self::collect_typeof_bindings(ty, &mut bindings);
                }
            }
            if let Some(judgment) = &premise.judgment {
                match judgment {
                    TypingJudgment::Ascription((term, t)) => {
                        Self::collect_typeof_bindings(t, &mut bindings);
                        bindings.insert(term.as_str());
                    }
                    TypingJudgment::Membership(var, _) => {
                        bindings.insert(var.as_str());
                    }
                    TypingJudgment::Operation { left, right, .. } => {
                        Self::collect_typeof_bindings(left, &mut bindings);
                        Self::collect_typeof_bindings(right, &mut bindings);
                    }
                    TypingJudgment::Equality { left, right } => {
                        Self::collect_typeof_bindings(left, &mut bindings);
                        Self::collect_typeof_bindings(right, &mut bindings);
                    }
                }
            }
        }

        if let Some(output) = &self.conclusion.context.output {
            for (var, ty) in &output.extensions {
                bindings.insert(var.as_str());
                Self::collect_typeof_bindings(ty, &mut bindings);
            }
        }
        Self::collect_typeof_bindings(&self.conclusion.kind, &mut bindings);

        bindings
    }

    fn collect_typeof_bindings<'a>(
        ty: &'a TypeExpr,
        out: &mut std::collections::HashSet<&'a str>,
    ) {
        match ty {
            TypeExpr::TypeOf(name) => {
                out.insert(name.as_str());
            }
            TypeExpr::Arrow(a, b) => {
                Self::collect_typeof_bindings(a, out);
                Self::collect_typeof_bindings(b, out);
            }
            TypeExpr::Union(items) => {
                for item in items {
                    Self::collect_typeof_bindings(item, out);
                }
            }
            TypeExpr::Not(inner) => Self::collect_typeof_bindings(inner, out),
            TypeExpr::Meta(_) | TypeExpr::Lit(_) | TypeExpr::Any
            | TypeExpr::None | TypeExpr::ContextExt(_) => {}
        }
    }

    /// Pretty multiline formatting as an inference rule
    pub fn pretty(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        let mut out = String::new();
        let conclusion_str = format!("{}", self.conclusion);

        if self.premises.is_empty() {
            out.push_str(&format!(
                "{}{}  [{}]",
                indent_str, conclusion_str, self.name
            ));
            return out;
        }

        let premise_lines: Vec<String> = self
            .premises
            .iter()
            .map(|p| format!("{}{}", indent_str, p))
            .collect();
        let max_width = premise_lines
            .iter()
            .map(|l| l.trim_start().len())
            .chain([conclusion_str.len()])
            .max()
            .unwrap_or(0);
        let bar = format!("{}{}", indent_str, "─".repeat(max_width.max(4)));

        out.push_str(&premise_lines.join("\n"));
        out.push('\n');
        out.push_str(&format!("{} [{}]", bar, self.name));
        out.push('\n');
        out.push_str(&format!("{}{}", indent_str, conclusion_str));
        out
    }
}

// =============================================================================
// PARSING
// =============================================================================

/// Parser for typing rules
pub struct RuleParser;

impl RuleParser {
    /// Parse a complete typing rule from premises string, conclusion string, and name
    pub fn parse(
        premises_str: &str,
        conclusion_str: &str,
        name: &str,
    ) -> Result<TypingRule, String> {
        let premises = premises_str
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .filter_map(|p| match Self::parse_premise(p) {
                Ok(Some(pr)) => Some(Ok(pr)),
                Ok(None) => None,
                Err(e) => Some(Err(e)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let conclusion = Self::parse_conclusion(conclusion_str)?;

        Ok(TypingRule {
            name: name.to_string(),
            premises,
            conclusion,
        })
    }

    /// Parse a conclusion: `Γ → Γ[x:τ] ⊢ τ` | `τ`
    pub fn parse_conclusion(s: &str) -> Result<Conclusion, String> {
        let s = s.trim();

        // Case 1: Contains ⊢ → context-transforming type conclusion
        if let Some((lhs, rhs)) = s.split_once('⊢') {
            let ty = TypeExpr::parse(rhs.trim())?;
            let ctx = Self::parse_conclusion_context(lhs.trim())?;
            return Ok(Conclusion {
                context: ctx,
                kind: ty,
            });
        }

        // Case 2: Bare type expression
        let ty = TypeExpr::parse(s)?;
        Ok(Conclusion {
            context: ConclusionContext::default(),
            kind: ty,
        })
    }

    /// Parse a premise: `x ∈ Γ` | `Γ ⊢ e : τ` | `τ₁ = τ₂` | `Γ[x:τ]`
    pub fn parse_premise(s: &str) -> Result<Option<Premise>, String> {
        let s = s.trim();
        if s.is_empty() {
            return Ok(None);
        }

        // Membership: x ∈ Γ
        if let Some((var, ctx)) = s.split_once('∈') {
            let var = var.trim().to_string();
            let ctx = ctx.trim().to_string();
            if var.is_empty() || ctx.is_empty() {
                return Err(format!("Invalid membership premise: '{}'", s));
            }
            return Ok(Some(Premise {
                setting: None,
                judgment: Some(TypingJudgment::Membership(var, ctx)),
            }));
        }

        // Typing judgment: Γ ⊢ e : τ
        if let Some((setting_part, ascr_part)) = s.split_once('⊢') {
            let setting = Self::parse_setting(setting_part.trim())?;
            let ascription = Self::parse_ascription(ascr_part.trim())?;
            return Ok(Some(Premise {
                setting: Some(setting),
                judgment: Some(TypingJudgment::Ascription(ascription)),
            }));
        }

        // Type operation: τ₁ = τ₂ or τ₁ ⊆ τ₂
        if let Some((left, op, right)) = Self::try_parse_operation(s) {
            return Ok(Some(Premise {
                setting: None,
                judgment: Some(TypingJudgment::Operation { left, op, right }),
            }));
        }

        // Setting-only premise
        let setting = Self::parse_setting(s)?;
        Ok(Some(Premise {
            setting: Some(setting),
            judgment: None,
        }))
    }

    /// Parse a type setting: `Γ` or `Γ[x:τ₁][y:τ₂]`
    pub fn parse_setting(s: &str) -> Result<TypeSetting, String> {
        let s = s.trim();
        if s.starts_with('[') && s.ends_with(']') && s.len() >= 2 {
            let inner = s[1..s.len() - 1].trim();
            let mut setting = Self::parse_setting(inner)?;
            setting.no_propagate = true;
            return Ok(setting);
        }
        if !s.contains('[') {
            return Ok(TypeSetting {
                name: s.to_string(),
                extensions: Vec::new(),
                no_propagate: false,
            });
        }

        let name_re = Regex::new(r"^\s*([^\[\s]+)\s*\[").map_err(|e| e.to_string())?;
        let name = name_re
            .captures(s)
            .and_then(|c| c.get(1))
            .map(|m| m.as_str().to_string())
            .ok_or_else(|| "Invalid setting: expected name before '['".to_string())?;

        let ext_re = Regex::new(r"\[([^:\]]+):([^\]]+)\]").map_err(|e| e.to_string())?;
        let mut extensions = Vec::new();
        for cap in ext_re.captures_iter(s) {
            let var = cap[1].trim().to_string();
            let ty = TypeExpr::parse(cap[2].trim())?;
            extensions.push((var, ty));
        }

        Ok(TypeSetting {
            name,
            extensions,
            no_propagate: false,
        })
    }

    /// Parse a type ascription: `e : τ`
    fn parse_ascription(s: &str) -> Result<TypeAscription, String> {
        let parts: Vec<&str> = s.splitn(2, ':').map(str::trim).collect();
        if parts.len() != 2 {
            return Err(format!(
                "Invalid ascription: expected 'term : type', got '{}'",
                s
            ));
        }
        let term = parts[0].to_string();
        let ty = TypeExpr::parse(parts[1])?;
        Ok((term, ty))
    }

    /// Try to parse a type operation: `τ₁ = τ₂` or `τ₁ ⊆ τ₂`
    fn try_parse_operation(s: &str) -> Option<(TypeExpr, TypeOperation, TypeExpr)> {
        // Try equality
        if let Some((l, r)) = s.split_once("=")
            && let (Ok(left), Ok(right)) =
                (TypeExpr::parse(l.trim()), TypeExpr::parse(r.trim()))
        {
            return Some((left, TypeOperation::Equality, right));
        }
        // Try inclusion (⊆ or <=)
        for sep in ["⊆ ", "<="] {
            if let Some((l, r)) = s.split_once(sep)
                && let (Ok(left), Ok(right)) =
                    (TypeExpr::parse(l.trim()), TypeExpr::parse(r.trim()))
            {
                return Some((left, TypeOperation::Inclusion, right));
            }
        }
        None
    }

    /// Parse the context part of a conclusion: `Γ → Γ[x:τ]`
    fn parse_conclusion_context(s: &str) -> Result<ConclusionContext, String> {
        if s.is_empty() {
            return Ok(ConclusionContext::default());
        }

        // Check for arrow (context transform)
        for arrow in ["→", "->"] {
            if let Some((l, r)) = s.split_once(arrow) {
                let input = if l.trim().is_empty() {
                    String::new()
                } else {
                    Self::parse_setting(l.trim())?.name
                };
                let output = if r.trim().is_empty() {
                    None
                } else {
                    Some(Self::parse_setting(r.trim())?)
                };
                return Ok(ConclusionContext { input, output });
            }
        }

        // No arrow: just input context
        let input = Self::parse_setting(s)?.name;
        Ok(ConclusionContext {
            input,
            output: None,
        })
    }
}

// Legacy API
impl TypingRule {
    pub fn new(premises: String, conclusion: String, name: String) -> Result<Self, String> {
        RuleParser::parse(&premises, &conclusion, &name)
    }

    pub fn parse_conclusion(s: &str) -> Result<Conclusion, String> {
        RuleParser::parse_conclusion(s)
    }
}

impl Conclusion {
    pub fn try_from_str(s: &str) -> Result<Self, String> {
        RuleParser::parse_conclusion(s)
    }

    pub fn try_from_string(s: String) -> Result<Self, String> {
        Self::try_from_str(&s)
    }
}

// =============================================================================
// DISPLAY
// =============================================================================

impl fmt::Display for TypeOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeOperation::Equality => write!(f, "="),
            TypeOperation::Inclusion => write!(f, "⊆"),
        }
    }
}

impl fmt::Display for TypeSetting {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let base = if self.extensions.is_empty() {
            self.name.clone()
        } else {
            let exts: Vec<String> = self
                .extensions
                .iter()
                .map(|(t, ty)| format!("{}:{}", t, ty))
                .collect();
            format!("{}[{}]", self.name, exts.join(", "))
        };

        if self.no_propagate {
            write!(f, "[{}]", base)
        } else {
            write!(f, "{}", base)
        }
    }
}

impl fmt::Display for TypingJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypingJudgment::Ascription((term, ty)) => write!(f, "{} : {}", term, ty),
            TypingJudgment::Membership(var, ctx) => write!(f, "{} ∈ {}", var, ctx),
            TypingJudgment::Operation { left, op, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
            TypingJudgment::Equality { left, right } => {
                write!(f, "{} = {}", left, right)
            }
        }
    }
}

impl fmt::Display for Premise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.setting, &self.judgment) {
            (Some(s), Some(j)) => write!(f, "{} ⊢ {}", s, j),
            (Some(s), None) => write!(f, "{}", s),
            (None, Some(j)) => write!(f, "{}", j),
            (None, None) => Ok(()),
        }
    }
}

impl fmt::Display for Conclusion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.context.is_empty() {
            write!(f, "{}", self.kind)
        } else {
            let input = &self.context.input;
            match (input.is_empty(), &self.context.output) {
                (false, Some(o)) => write!(f, "{} → {} ⊢ {}", input, o, self.kind),
                (false, None) => write!(f, "{} ⊢ {}", input, self.kind),
                (true, Some(o)) => write!(f, "{} → {} ⊢ {}", o.name, o, self.kind),
                (true, None) => write!(f, "{}", self.kind),
            }
        }
    }
}

impl fmt::Display for TypingRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.premises.is_empty() {
            write!(f, "[{}] {}", self.name, self.conclusion)
        } else {
            let premises: Vec<String> = self.premises.iter().map(|p| p.to_string()).collect();
            write!(
                f,
                "[{}] {} ⇒ {}",
                self.name,
                premises.join(", "),
                self.conclusion
            )
        }
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_app_rule_with_metas() {
        let rule = TypingRule::new(
            "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A".into(),
            "?B".into(),
            "app".into(),
        )
        .unwrap();

        assert_eq!(rule.name, "app");
        assert_eq!(rule.premises.len(), 2);
        assert!(rule.conclusion.kind.has_metas());
    }

    #[test]
    fn parse_var_rule() {
        let rule = TypingRule::new(
            "x ∈ Γ".into(),
            "Γ(x)".into(),
            "var".into(),
        )
        .unwrap();

        assert_eq!(rule.name, "var");
        assert_eq!(rule.premises.len(), 1);
        assert_eq!(rule.conclusion.kind, TypeExpr::ContextExt("x".into()));
    }

    #[test]
    fn parse_lambda_rule_with_context_extension() {
        let rule = TypingRule::new(
            "Γ[a:'A'] ⊢ e : ?R".into(),
            "'A' -> ?R".into(),
            "lambda".into(),
        )
        .unwrap();

        assert_eq!(rule.name, "lambda");
        assert_eq!(rule.premises.len(), 1);
        let premise = &rule.premises[0];
        let setting = premise.setting.as_ref().unwrap();
        assert_eq!(setting.name, "Γ");
        assert_eq!(setting.extensions.len(), 1);
        assert_eq!(setting.extensions[0].0, "a");
        assert_eq!(setting.extensions[0].1, TypeExpr::Lit("A".into()));
    }

    #[test]
    fn parse_define_rule_with_context_transform() {
        let rule = TypingRule::new(
            "Γ ⊢ value : ?T".into(),
            "Γ → Γ[name:?T] ⊢ 'Unit'".into(),
            "define".into(),
        )
        .unwrap();

        assert_eq!(rule.name, "define");
        assert_eq!(rule.premises.len(), 1);
        let ctx = &rule.conclusion.context;
        assert_eq!(ctx.input, "Γ");
        let output = ctx.output.as_ref().unwrap();
        assert_eq!(output.extensions.len(), 1);
        assert_eq!(output.extensions[0].0, "name");
        assert_eq!(output.extensions[0].1, TypeExpr::Meta("T".into()));
        assert_eq!(rule.conclusion.kind, TypeExpr::Lit("Unit".into()));
    }

    #[test]
    fn compilation_eliminates_metas() {
        let rule = TypingRule::new(
            "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A".into(),
            "?B".into(),
            "app".into(),
        ).unwrap();
        let compiled = CompilationPass::compile(&rule).unwrap();
        // Original named Metas (?A, ?B) are replaced with fresh internal metas (_0, _1, …)
        let all_metas = compiled.conclusion.kind.metas();
        assert!(!all_metas.iter().any(|m| *m == "A" || *m == "B"),
            "original user-named metas should be gone, got {:?}", all_metas);
    }

    #[test]
    fn compiled_app_rule_has_typeof_constraints() {
        let rule = TypingRule::new(
            "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A".into(),
            "?B".into(),
            "app".into(),
        ).unwrap();
        let compiled = CompilationPass::compile(&rule).unwrap();
        // After compilation, fresh metas and typeof/equality constraints are present
        assert!(!compiled.premises.is_empty(), "should have premises");
        let texts: Vec<_> = compiled.premises.iter().map(|p| format!("{}", p)).collect();
        let j = texts.join("\n");
        // At least one equality or typeof constraint should exist
        assert!(j.contains("=") || j.contains("typeof"), "premises should have constraints: {}", j);
    }

    #[test]
    fn rule_with_single_meta_compiles() {
        let rule = TypingRule::new(
            "Γ ⊢ e : ?R".into(),
            "?R".into(),
            "single".into(),
        ).unwrap();
        let compiled = CompilationPass::compile(&rule).unwrap();
        // Meta("R") is replaced with a fresh meta, top-level so typeof(e) = _0 is added
        let has_typeof_e = compiled.premises.iter().any(|p| {
            let s = format!("{}", p);
            s.contains("typeof(e)")
        });
        assert!(has_typeof_e, "should have typeof(e) constraint");
    }
}
