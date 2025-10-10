// In this file we implement the actuall constraint engine
// Basically, we have a function that takes
// logits and a vocab as input and outputs a *valid* subset of logits
// V: L -> V where V << L

use crate::logic::grammar::Grammar;

struct Engine {
    vocab: Vec<String>,
    grammar: Grammar,
}