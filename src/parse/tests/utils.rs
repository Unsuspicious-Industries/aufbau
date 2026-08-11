use crate::engine::grammar::SPG;
use crate::engine::parse::TypedParser;
use crate::semantics::TypingRuntime;
use crate::typing::TypingDomain;

/// Parser over a real typing runtime. Grammars without typing rules finalize
/// every node to TOP, so this is the structural-only parsing harness.
pub(crate) fn stub_parser(grammar: SPG) -> TypedParser {
    let runtime = TypingRuntime::new(TypingDomain::default(), grammar.clone());
    TypedParser::new(grammar, runtime)
}
