use crate::logic::grammar::Nonterminal;
use std::collections::HashMap;

/// A sophisticated recursion detection and prevention system for recursive descent parsing.
///
/// This tracker monitors call stacks, detects left-recursion cycles, implements memoization
/// to avoid redundant parsing attempts, and provides comprehensive debugging capabilities.
pub struct RecursionTracker {
    /// Call stack tracking (nonterminal, position) pairs to detect cycles
    call_stack: Vec<(Nonterminal, usize)>,

    /// Memoization table to cache parsing results and avoid redundant work
    /// Maps (nonterminal, position) -> Optional(result, new_position)
    memo_table: HashMap<(Nonterminal, usize), Option<(String, usize)>>,

    /// Configuration limits
    max_recursion_depth: usize,
    max_backtrack_attempts: usize,

    /// Runtime counters
    backtrack_attempts: usize,
}

impl RecursionTracker {
    /// Create a new recursion tracker with sensible defaults
    pub fn new() -> Self {
        Self {
            call_stack: Vec::new(),
            memo_table: HashMap::new(),
            max_recursion_depth: 100, // More conservative default
            max_backtrack_attempts: 1000,
            backtrack_attempts: 0,
        }
    }

    /// Create a recursion tracker with custom limits
    pub fn with_limits(max_depth: usize, max_backtrack: usize) -> Self {
        Self {
            call_stack: Vec::new(),
            memo_table: HashMap::new(),
            max_recursion_depth: max_depth,
            max_backtrack_attempts: max_backtrack,
            backtrack_attempts: 0,
        }
    }

    /// Reset the tracker state for a new parsing session
    pub fn reset(&mut self) {
        self.call_stack.clear();
        self.memo_table.clear();
        self.backtrack_attempts = 0;
    }

    /// Check if entering this nonterminal at the current position would create a cycle
    pub fn would_create_cycle(&self, nt: &Nonterminal, pos: usize) -> bool {
        self.call_stack
            .iter()
            .any(|(seen_nt, seen_pos)| seen_nt == nt && *seen_pos == pos)
    }

    /// Check if recursion depth limit has been exceeded
    pub fn exceeds_depth_limit(&self) -> bool {
        self.call_stack.len() >= self.max_recursion_depth
    }

    /// Enter a new parsing context (push to call stack)
    /// Returns an error if this would violate recursion constraints
    pub fn enter(&mut self, nt: &Nonterminal, pos: usize) -> Result<(), String> {
        // Check for depth limit
        if self.exceeds_depth_limit() {
            return Err(format!(
                "Recursion depth limit ({}) exceeded while parsing '{}'",
                self.max_recursion_depth, nt
            ));
        }

        // Check for left-recursion cycle
        if self.would_create_cycle(nt, pos) {
            crate::debug_debug!(
                "recursion_tracker",
                "CYCLE DETECTED: {} at pos {} already in call stack",
                nt,
                pos
            );
            return Err(format!(
                "Left recursion detected: '{}' at position {} creates a cycle",
                nt, pos
            ));
        }

        // Safe to enter
        self.call_stack.push((nt.clone(), pos));
        crate::debug_trace!(
            "recursion_tracker",
            "Entered: {} at pos {} (depth: {})",
            nt,
            pos,
            self.call_stack.len()
        );

        Ok(())
    }

    /// Exit the current parsing context (pop from call stack)
    pub fn exit(&mut self) {
        if let Some((nt, pos)) = self.call_stack.pop() {
            crate::debug_trace!(
                "recursion_tracker",
                "Exited: {} at pos {} (depth: {})",
                nt,
                pos,
                self.call_stack.len()
            );
        }
    }

    /// Record a backtrack attempt
    pub fn record_backtrack(&mut self) -> Result<(), String> {
        self.backtrack_attempts += 1;
        if self.backtrack_attempts >= self.max_backtrack_attempts {
            Err(format!(
                "Backtracking limit ({}) exceeded - possible infinite exploration",
                self.max_backtrack_attempts
            ))
        } else {
            Ok(())
        }
    }

    /// Get current recursion depth
    pub fn depth(&self) -> usize {
        self.call_stack.len()
    }

    /// Get formatted call stack for debugging
    pub fn call_stack_trace(&self) -> String {
        self.call_stack
            .iter()
            .map(|(nt, pos)| format!("{}@{}", nt, pos))
            .collect::<Vec<_>>()
            .join(" -> ")
    }

    /// Check if we have memoized results for this (nonterminal, position) pair
    pub fn has_memo(&self, nt: &Nonterminal, pos: usize) -> bool {
        self.memo_table.contains_key(&(nt.clone(), pos))
    }

    /// Get memoized result if available
    pub fn get_memo(&self, nt: &Nonterminal, pos: usize) -> Option<&Option<(String, usize)>> {
        self.memo_table.get(&(nt.clone(), pos))
    }

    /// Store a result in the memoization table
    pub fn store_memo(&mut self, nt: &Nonterminal, pos: usize, result: Option<(String, usize)>) {
        self.memo_table.insert((nt.clone(), pos), result);
    }

    /// Get runtime statistics
    pub fn stats(&self) -> RecursionStats {
        RecursionStats {
            current_depth: self.call_stack.len(),
            max_depth_reached: self
                .max_recursion_depth
                .saturating_sub(self.max_recursion_depth - self.call_stack.len()),
            backtrack_attempts: self.backtrack_attempts,
            memo_entries: self.memo_table.len(),
        }
    }
}

/// Statistics about recursion tracker usage
#[derive(Debug, Clone)]
pub struct RecursionStats {
    pub current_depth: usize,
    pub max_depth_reached: usize,
    pub backtrack_attempts: usize,
    pub memo_entries: usize,
}

impl Default for RecursionTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// RAII guard for automatic entry/exit management
pub struct ParseContext<'a> {
    tracker: &'a mut RecursionTracker,
}

impl<'a> ParseContext<'a> {
    /// Create a new parse context, automatically entering the tracker
    pub fn new(
        tracker: &'a mut RecursionTracker,
        nt: &Nonterminal,
        pos: usize,
    ) -> Result<Self, String> {
        tracker.enter(nt, pos)?;
        Ok(Self { tracker })
    }
}

impl<'a> Drop for ParseContext<'a> {
    fn drop(&mut self) {
        self.tracker.exit();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cycle_detection() {
        let mut tracker = RecursionTracker::new();
        let nt = Nonterminal::from("Expr");

        // First entry should succeed
        assert!(tracker.enter(&nt, 0).is_ok());

        // Second entry at same position should detect cycle
        assert!(tracker.would_create_cycle(&nt, 0));
        assert!(tracker.enter(&nt, 0).is_err());

        // Different position should be ok
        assert!(tracker.enter(&nt, 1).is_ok());

        tracker.exit(); // pos 1
        tracker.exit(); // pos 0
    }

    #[test]
    fn test_depth_limit() {
        let mut tracker = RecursionTracker::with_limits(2, 100);
        let nt1 = Nonterminal::from("A");
        let nt2 = Nonterminal::from("B");
        let nt3 = Nonterminal::from("C");

        assert!(tracker.enter(&nt1, 0).is_ok());
        assert!(tracker.enter(&nt2, 1).is_ok());
        assert!(tracker.enter(&nt3, 2).is_err()); // Should exceed limit
    }

    #[test]
    fn test_raii_context() {
        let mut tracker = RecursionTracker::new();
        let nt = Nonterminal::from("Test");

        assert_eq!(tracker.depth(), 0);

        {
            let _ctx = ParseContext::new(&mut tracker, &nt, 0).unwrap();
            // Note: can't check depth here due to borrow checker
        } // Context drops here, should auto-exit

        assert_eq!(tracker.depth(), 0);
    }
}
