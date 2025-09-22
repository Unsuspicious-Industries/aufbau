use std::fmt::Display;

use super::*;

impl Display for PartialOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PartialOutcome::Incomplete { states } => {
                write!(f, "Incomplete: states {}", states.iter().map(|s| s.to_string()).collect::<Vec<_>>().join(", "))
            }
            PartialOutcome::Complete { node } => {
                write!(f, "Complete: {}", node.show_simple())
            }
            PartialOutcome::Expandable { node } => {
                write!(f, "Expandable: {}", node.show_simple())
            }
            PartialOutcome::Error(err) => {
                write!(f, "Error: {}", err)
            }
        }
    }
}

impl Display for PartialState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let current_index = self.final_production.current_index();
        let total = self.final_production.total_symbols();
        let parsed_count = self.final_production.parsed.len();
        let partial_count = self.final_production.partial.len();
        let remaining_count = self.final_production.remaining.len();
        
        // Show what symbols are remaining (to help with "expected" text)
        let remaining_symbols: Vec<String> = self.final_production.remaining.iter()
            .map(|s| format!("'{}'", s.value()))
            .collect();
        let remaining_text = if !remaining_symbols.is_empty() {
            format!(" expected {}", remaining_symbols.join(", "))
        } else {
            String::new()
        };
        
        write!(
            f,
            "PartialState[parsed:{}, partial:{}, remaining:{}] production: {:?} ({} of {}), AST: {}{}",
            parsed_count,
            partial_count,
            remaining_count,
            self.final_production.production,
            current_index + 1,
            total,
            self.ast.show_simple(),
            remaining_text
        )
    }
}