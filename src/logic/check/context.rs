use std::collections::HashMap;
use crate::logic::bind::typing::BoundType;



#[derive(Debug, Clone, PartialEq)]
pub struct TypingContext {
    /// Variables in this context level
    bindings: HashMap<String, BoundType>,
    /// Parent context that this context can see variables from
    parent: Option<Box<TypingContext>>,
}

impl TypingContext {
    /// Create a new empty context
    pub fn new() -> Self { 
        Self { 
            bindings: HashMap::new(),
            parent: None,
        } 
    }

    /// Create a new context with the given references
    pub fn with_references<I: IntoIterator<Item=(String, BoundType)>>(iter: I) -> Self {
        let mut bindings = HashMap::new();
        for (k, v) in iter {
            bindings.insert(k, v);
        }
        Self { 
            bindings,
            parent: None,
        }
    }

    /// Create a new child context with this context as parent
    /// The child context can see variables from this context but not vice versa
    pub fn create_child(&self) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }

    /// Create a new child context with the given bindings
    pub fn create_child_with<I: IntoIterator<Item=(String, BoundType)>>(&self, iter: I) -> Self {
        let mut bindings = HashMap::new();
        for (k, v) in iter {
            bindings.insert(k, v);
        }
        Self {
            bindings,
            parent: Some(Box::new(self.clone())),
        }
    }

    /// Add a single reference to this context
    pub fn add(&mut self, var: String, ty: BoundType) {
        self.bindings.insert(var, ty);
    }

    /// Extend this context with new references
    pub fn extend<I: IntoIterator<Item=(String, BoundType)>>(&mut self, iter: I) { 
        for (k, v) in iter { 
            self.bindings.insert(k, v); 
        } 
    }

    /// Look up a variable, searching in this context then parent contexts
    pub fn lookup(&self, v: &str) -> Option<&BoundType> { 
        // First search in this context's bindings
        if let Some(ty) = self.bindings.get(v) {
            return Some(ty);
        }
        
        // If not found, search in parent context
        if let Some(parent) = &self.parent {
            parent.lookup(v)
        } else {
            None
        }
    }

    /// Check if a variable is bound in this context only (not parent contexts)
    pub fn is_bound_in_current_context(&self, v: &str) -> bool {
        self.bindings.contains_key(v)
    }

    /// Get all references from this context and all parent contexts (inner contexts shadow outer ones)
    /// 
    /// **WARNING**: This is expensive for deep hierarchies as it traverses the entire parent chain
    /// and creates a new HashMap. Prefer using `lookup()` for individual variable lookups.
    /// This method is mainly useful for debugging or when you actually need all variables.
    pub fn all_references(&self) -> HashMap<String, &BoundType> {
        let mut result = HashMap::new();
        self.collect_all_references(&mut result);
        result
    }
    
    /// Efficiently collect all references by traversing the hierarchy once
    fn collect_all_references<'a>(&'a self, result: &mut HashMap<String, &'a BoundType>) {
        // First collect from parent (so child bindings can override)
        if let Some(parent) = &self.parent {
            parent.collect_all_references(result);
        }
        
        // Then add/override with this context's bindings
        for (var, ty) in &self.bindings {
            result.insert(var.clone(), ty);
        }
    }

    /// Get references from this context only (not parent contexts)
    pub fn current_context_references(&self) -> &HashMap<String, BoundType> {
        &self.bindings
    }

    /// Create a new child context with the given references
    /// This replaces the old with_extended_scope method
    pub fn with_extended_scope<I: IntoIterator<Item=(String, BoundType)>>(&self, iter: I) -> Self {
        self.create_child_with(iter)
    }

    /// Dump the full hierarchical context chain (ancestor first) suitable for TRACE level
    pub fn dump(&self) -> String {
        let mut chain: Vec<&TypingContext> = Vec::new();
        self.collect_chain(&mut chain);
        let mut out = String::new();
        for (depth, ctx) in chain.iter().enumerate() {
            use std::fmt::Write;
            let _ = writeln!(out, "#{} {{", depth);
            if ctx.bindings.is_empty() {
                let _ = writeln!(out, "  <empty>");
            } else {
                for (k, v) in ctx.bindings.iter() {
                    let _ = writeln!(out, "  {} : {:?}", k, v);
                }
            }
            let _ = writeln!(out, "}}\n");
        }
        out
    }

    fn collect_chain<'a>(&'a self, out: &mut Vec<&'a TypingContext>) {
        if let Some(parent) = &self.parent { parent.collect_chain(out); }
        out.push(self);
    }
}


impl BoundType {

    pub fn resolve(&mut self, context: &TypingContext) -> () {
        println!("Resolving type: {:?}", self);
        if let BoundType::ContextCall(_ctx, var) = self {
            if let Some(bound) = context.lookup(var).cloned() {
                println!("Resolved context call {} to type {}", var, bound);
                *self = bound;
            }
        }
    }
}