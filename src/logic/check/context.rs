use std::collections::HashMap;
use crate::logic::typing::Type;



#[derive(Debug, Clone, PartialEq)]
pub struct TypingContext {
    /// Stack of scopes, where the last element is the innermost scope
    scopes: Vec<HashMap<String, Type>>,
}

impl TypingContext {
    /// Create a new context with a single empty scope
    pub fn new() -> Self { 
        Self { 
            scopes: vec![HashMap::new()] 
        } 
    }

    /// Create a new context with the given references in the initial scope
    pub fn with_references<I: IntoIterator<Item=(String, Type)>>(iter: I) -> Self {
        let mut initial_scope = HashMap::new();
        for (k, v) in iter {
            initial_scope.insert(k, v);
        }
        Self { 
            scopes: vec![initial_scope] 
        }
    }

    /// Enter a new scope (pushes an empty scope onto the stack)
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope (pops the innermost scope from the stack)
    /// Returns an error if trying to exit the last scope
    pub fn exit_scope(&mut self) -> Result<(), String> {
        if self.scopes.len() <= 1 {
            return Err("Cannot exit the last scope".to_string());
        }
        self.scopes.pop();
        Ok(())
    }

    /// Get the current scope depth (number of nested scopes)
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Extend the current (innermost) scope with new references
    pub fn extend<I: IntoIterator<Item=(String, Type)>>(&mut self, iter: I) { 
        let current_scope = self.scopes.last_mut().expect("Context should always have at least one scope");
        for (k, v) in iter { 
            current_scope.insert(k, v); 
        } 
    }

    /// Add a single reference to the current scope
    pub fn bind(&mut self, var: String, ty: Type) {
        let current_scope = self.scopes.last_mut().expect("Context should always have at least one scope");
        current_scope.insert(var, ty);
    }

    /// Look up a variable, searching from innermost to outermost scope
    pub fn lookup(&self, v: &str) -> Option<&Type> { 
        // Search from innermost scope (end of vector) to outermost (beginning)
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(v) {
                return Some(ty);
            }
        }
        None
    }

    /// Check if a variable is bound in the current (innermost) scope only
    pub fn is_bound_in_current_scope(&self, v: &str) -> bool {
        self.scopes
            .last()
            .map(|scope| scope.contains_key(v))
            .unwrap_or(false)
    }

    /// Get all references from all scopes (innermost references shadow outer ones)
    pub fn all_references(&self) -> HashMap<String, &Type> {
        let mut result = HashMap::new();
        // Start from outermost scope and work inward so inner references override outer ones
        for scope in self.scopes.iter() {
            for (var, ty) in scope {
                result.insert(var.clone(), ty);
            }
        }
        result
    }

    /// Get references from the current scope only
    pub fn current_scope_references(&self) -> &HashMap<String, Type> {
        self.scopes.last().expect("Context should always have at least one scope")
    }

    /// Create a new context that extends this one with a new scope containing the given references
    pub fn with_extended_scope<I: IntoIterator<Item=(String, Type)>>(&self, iter: I) -> Self {
        let mut new_context = self.clone();
        new_context.enter_scope();
        new_context.extend(iter);
        new_context
    }
}
