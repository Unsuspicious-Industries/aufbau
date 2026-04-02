use crate::logic::parse::arena::{CtxId, FrontierId, NodeId, NtId, ProdId, TypeId};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct DepthMeta {
    pub searched_depth: u16,
    pub hit_depth_limit: bool,
    pub depth_failures: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrontierItem {
    pub nt: NtId,
    pub prod: ProdId,
    pub dot: u16,
    pub input_idx: u32,
    pub ctx: CtxId,
    pub expected: Option<TypeId>,
    pub depth: u16,
    pub children: Vec<NodeId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedPrefixState {
    pub input_len: usize,
    pub roots: Vec<NodeId>,
    pub frontier: Vec<FrontierId>,
    pub depth: DepthMeta,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedPrefixError {
    pub input_len: usize,
    pub depth: DepthMeta,
    pub message: String,
}

impl std::fmt::Display for TypedPrefixError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for TypedPrefixError {}

impl TypedPrefixError {
    /// Time: O(1). Space: O(1).
    pub fn too_deep(input_len: usize, searched_depth: u16, depth_failures: u32) -> Self {
        Self {
            input_len,
            depth: DepthMeta {
                searched_depth,
                hit_depth_limit: true,
                depth_failures,
            },
            message: "all branches exceeded depth budget".into(),
        }
    }

    /// Time: O(1). Space: O(1).
    pub fn rejected(input_len: usize, searched_depth: u16, message: impl Into<String>) -> Self {
        Self {
            input_len,
            depth: DepthMeta {
                searched_depth,
                hit_depth_limit: false,
                depth_failures: 0,
            },
            message: message.into(),
        }
    }
}
