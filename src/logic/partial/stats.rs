use crate::logic::partial::structure::SppfNodeId;
use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
pub struct GlobalCacheStats {
    pub grammar_count: usize,
    pub node_pool_count: usize,
    pub total_nodes: usize,
    pub unique_nodes: usize,
    pub duplicate_nodes: usize,
    pub input_cache_entries: usize,
}

#[derive(Clone, Debug, Serialize)]
pub struct GrammarCacheStats {
    pub grammar: String,
    pub node_count: usize,
    pub unique_nodes: usize,
    pub duplicate_nodes: usize,
}

#[derive(Clone, Debug, Serialize)]
pub struct InputCacheEntry {
    pub grammar: String,
    pub input: String,
    pub nonterminal: String,
    pub node_id: SppfNodeId,
}
