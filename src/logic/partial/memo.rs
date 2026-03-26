use crate::logic::partial::structure::SppfNodeId;
use serde::Serialize;
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

const DEFAULT_SHARED_MEMO_ENTRY_LIMIT: usize = 8192;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub(crate) struct ParseMemoKey {
    pub input_id: u64,
    pub nt_name: String,
    pub binding: Option<String>,
    pub abs_pos: usize,
    pub level: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub(crate) struct ParsedNt {
    pub node_id: SppfNodeId,
    pub consumed: usize,
    pub complete: bool,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct MemoEntry {
    pub complete: Vec<ParsedNt>,
    pub partial: Vec<ParsedNt>,
}

pub(crate) type MemoTable = HashMap<ParseMemoKey, MemoEntry>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct SharedMemoKey {
    pub grammar: String,
    pub input: String,
    pub max_recursion: usize,
}

static SHARED_MEMO: OnceLock<Mutex<HashMap<SharedMemoKey, MemoTable>>> = OnceLock::new();

impl MemoEntry {
    pub fn from_outcomes(outcomes: Vec<ParsedNt>) -> Self {
        let (complete, partial): (Vec<_>, Vec<_>) =
            outcomes.into_iter().partition(|out| out.complete);
        Self { complete, partial }
    }

    pub fn all(&self) -> Vec<ParsedNt> {
        self.complete
            .iter()
            .chain(self.partial.iter())
            .cloned()
            .collect()
    }

    pub fn stable_only(&self) -> Self {
        Self {
            complete: self.complete.clone(),
            partial: Vec::new(),
        }
    }

    pub fn has_partial(&self) -> bool {
        !self.partial.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.complete.is_empty() && self.partial.is_empty()
    }
}

pub(crate) fn stable_memo(table: &MemoTable) -> MemoTable {
    table
        .iter()
        .filter_map(|(key, entry)| {
            let stable = entry.stable_only();
            (!stable.is_empty()).then(|| (key.clone(), stable))
        })
        .collect()
}

pub(crate) fn shared_memo_get(key: &SharedMemoKey) -> Option<MemoTable> {
    shared_memo_store()
        .lock()
        .expect("shared memo poisoned")
        .get(key)
        .cloned()
}

pub(crate) fn shared_memo_put(key: SharedMemoKey, table: MemoTable) {
    let mut store = shared_memo_store().lock().expect("shared memo poisoned");
    store.insert(key, limit_entries(table));
}

pub(crate) fn clear_shared_memo() {
    shared_memo_store()
        .lock()
        .expect("shared memo poisoned")
        .clear();
}

fn shared_memo_store() -> &'static Mutex<HashMap<SharedMemoKey, MemoTable>> {
    SHARED_MEMO.get_or_init(|| Mutex::new(HashMap::new()))
}

fn limit_entries(table: MemoTable) -> MemoTable {
    if table.len() <= DEFAULT_SHARED_MEMO_ENTRY_LIMIT {
        return table;
    }

    table
        .into_iter()
        .take(DEFAULT_SHARED_MEMO_ENTRY_LIMIT)
        .collect()
}
