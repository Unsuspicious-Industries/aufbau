use crate::logic::partial::structure::SppfNodeId;
use serde::Serialize;
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub(crate) struct ParseMemoKey {
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

static SHARED_MEMO: OnceLock<Mutex<()>> = OnceLock::new();

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

pub(crate) fn clear_shared_memo() {
    let _guard = shared_memo_store().lock().expect("shared memo poisoned");
}

fn shared_memo_store() -> &'static Mutex<()> {
    SHARED_MEMO.get_or_init(|| Mutex::new(()))
}
