// Custom engine for automata
// drawings from asciiflow.com

use crate::regex::{Regex, dfa::DFA};

// Core types
type StateId = usize;

// NFA
#[derive(Debug, Clone)]
pub struct NFA {
    pub states: Vec<NFAState>,
    pub start: StateId,
    pub accept: StateId,
}

#[derive(Debug, Clone)]
pub struct NFAState {
    // Use Vec for simplicity, HashMap if you need to deduplicate
    pub epsilon: Vec<StateId>,
    pub transitions: Vec<(char, StateId)>,
}

impl NFAState {
    pub fn new() -> Self {
        NFAState {
            epsilon: Vec::new(),
            transitions: Vec::new(),
        }
    }

    pub fn add_epsilon(&mut self, state: StateId) {
        self.epsilon.push(state);
    }

    pub fn add_transition(&mut self, symbol: char, state: StateId) {
        self.transitions.push((symbol, state));
    }
}

impl NFA {
    pub fn new() -> Self {
        NFA {
            states: Vec::new(),
            start: 0,
            accept: 0,
        }
    }

    pub fn epsilon_closure(&self, states: Vec<StateId>) -> Vec<StateId> {
        let mut closure = states.clone();
        let mut stack = states;

        while let Some(state) = stack.pop() {
            for &next_state in &self.states[state].epsilon {
                if !closure.contains(&next_state) {
                    closure.push(next_state);
                    stack.push(next_state);
                }
            }
        }

        closure
    }

    pub fn add_state(&mut self) -> StateId {
        let id = self.states.len();
        self.states.push(NFAState::new());
        id
    }

    pub fn add_epsilon(&mut self, from: StateId, to: StateId) {
        self.states[from].add_epsilon(to);
    }

    pub fn add_transition(&mut self, from: StateId, symbol: char, to: StateId) {
        self.states[from].add_transition(symbol, to);
    }

    pub fn from(r: Regex) -> Self {
        let mut nfa = NFA::new();
        let start = nfa.add_state();
        let accept = nfa.build_from_regex(r, start);
        nfa.accept = accept;
        nfa
    }

    pub fn accepts(&self, input: &str) -> bool {
        let dfa = DFA::from(self.clone());
        dfa.accepts(input)
    }

    pub fn build_from_regex(&mut self, r: Regex, start: StateId) -> StateId {
        // !!
        // NFA construction follows Thompson's algorithm
        match r {
            Regex::Empty => {
                // Empty regex accepts nothing - create unreachable dead end
                let deadend = self.add_state();
                deadend
            }
            Regex::Epsilon => start, // Epsilon accepts empty string
            Regex::Char(c) => {
                let end = self.add_state();
                self.add_transition(start, c, end);
                end
            }
            Regex::Union(l, r) => {
                /*
                          ┌────────┐
                        ┌►│..LEFT..├─┐
                ┌─────┬─┘ └────────┘ └─►┌───┐
                │START│                 │END│
                └─────┴┐  ┌─────────┐ ┌►└───┘
                       └─►┤..RIGHT..├─┘
                          └─────────┘
                 */
                let sl = self.build_from_regex(*l, start); // left branch
                let sr = self.build_from_regex(*r, start); // right branch

                // unification
                let end = self.add_state();
                self.add_epsilon(sl, end);
                self.add_epsilon(sr, end);
                end
            }
            Regex::Concat(l, r) => {
                /*
                          ┌────────┐
                        ┌►│..LEFT..├─┐
                ┌─────┐ε│ └────────┘ │ε
                │START├─┘  ┌─────────┘  ┌─►
                └─────┘    │ ┌─────────┐│
                           └►│..RIGHT..│┘
                             └─────────┘
                */
                let mid = self.build_from_regex(*l, start);
                // wire toghether
                let end = self.build_from_regex(*r, mid);
                end
            }
            Regex::Star(r) => {
                /*
                               ε
                       ┌─────┬────►┌──────────┐
                       │start│     │...LOOP...│
                       └─────┘◄────└──────────┘
                               ε
                */
                let loop_end = self.build_from_regex(*r, start);
                self.add_epsilon(start, loop_end);
                self.add_epsilon(loop_end, start);
                start
            }
            Regex::Range(s, l) => {
                /*
                         l
                    ┌───────────┐
                    │   l-1     │
                    │ ┌───────┐ │
                    │ │ l-2   ▼ ▼
                ┌──┴─┴┬────►┌───┐
                │start│...  │END│
                └──┬─┬┴────►└───┘
                    │ │ s+2   ▲ ▲
                    │ └───────┘ │
                    │   s+1     │
                    └───────────┘
                         s
                 */

                let end = self.add_state();
                for i in s..=l {
                    self.add_transition(start, i, end);
                }
                end
            }
        }
    }
}

use std::fmt;

impl fmt::Display for NFA {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "NFA with {} states", self.states.len())?;
        writeln!(f, "Start: {}, Accept: {}", self.start, self.accept)?;
        writeln!(f)?;

        for (id, state) in self.states.iter().enumerate() {
            write!(f, "State {}", id)?;
            if id == self.start {
                write!(f, " (START)")?;
            }
            if id == self.accept {
                write!(f, " (ACCEPT)")?;
            }
            writeln!(f)?;

            // Show transitions from this state
            if state.transitions.is_empty() && state.epsilon.is_empty() {
                writeln!(f, "  (no transitions)")?;
            } else {
                for (symbol, to) in &state.transitions {
                    writeln!(f, "  └─'{}'───> ({})", *symbol as char, to)?;
                }
                for to in &state.epsilon {
                    writeln!(f, "  └─ε───> ({})", to)?;
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_from_regex() {
        let regex = Regex::from_str("(a|b)bb").unwrap();
        let automata = NFA::from(regex);
        println!("{}", automata);
    }
}
