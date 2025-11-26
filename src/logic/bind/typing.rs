use crate::logic::partial::NonTerminal;

#[derive(Debug, Clone, PartialEq)]
pub enum BoundType {
    Atom(String),
    Arrow(Box<BoundType>, Box<BoundType>),
    Tuple(Vec<BoundType>),
    Pointer(Box<BoundType>),
    Array(Box<BoundType>, u64),
    Not(Box<BoundType>),
    Intersection(Box<BoundType>, Box<BoundType>),
    Union(Box<BoundType>, Box<BoundType>),
    ContextCall(String, String),
    Universe,
    Empty,
}

impl BoundType {
    pub fn is_compatible_with(&self, other: &BoundType) -> bool {
        self.is_subtype_of(other) || other.is_subtype_of(self)
    }

    pub fn is_subtype_of(&self, other: &BoundType) -> bool {
        use BoundType::*;

        match (self, other) {
            (a, b) if a == b => true,
            (_, Universe) => true,
            (Empty, _) => true,
            (Arrow(a1, a2), Arrow(b1, b2)) => b1.is_subtype_of(a1) && a2.is_subtype_of(b2),
            (Tuple(a_elems), Tuple(b_elems)) => {
                a_elems.len() == b_elems.len()
                    && a_elems
                        .iter()
                        .zip(b_elems.iter())
                        .all(|(a, b)| a.is_subtype_of(b))
            }

            (Pointer(a), Pointer(b)) => a.is_subtype_of(b),
            (Array(a, n), Array(b, m)) => a.is_subtype_of(b) && n == m,
            (t, Union(u1, u2)) => t.is_subtype_of(u1) || t.is_subtype_of(u2),
            (Union(u1, u2), t) => u1.is_subtype_of(t) && u2.is_subtype_of(t),
            (Intersection(i1, i2), t) => i1.is_subtype_of(t) || i2.is_subtype_of(t),
            (t, Intersection(i1, i2)) => t.is_subtype_of(i1) && t.is_subtype_of(i2),
            (t, Not(n)) => !t.overlaps_with(n),
            (ContextCall(ctx1, var1), ContextCall(ctx2, var2)) => ctx1 == ctx2 && var1 == var2,
            _ => false,
        }
    }

    pub fn overlaps_with(&self, other: &BoundType) -> bool {
        use BoundType::*;
        match (self, other) {
            (a, b) if a == b => true,
            (Universe, Empty) | (Empty, Universe) => false,
            (Universe, _) | (_, Universe) => true,
            (Empty, _) | (_, Empty) => false,
            (Union(u1, u2), t) | (t, Union(u1, u2)) => u1.overlaps_with(t) || u2.overlaps_with(t),
            (Intersection(i1, i2), t) | (t, Intersection(i1, i2)) => {
                i1.overlaps_with(t) && i2.overlaps_with(t)
            }
            (Arrow(a1, a2), Arrow(b1, b2)) => a1.overlaps_with(b1) && a2.overlaps_with(b2),
            (Pointer(a), Pointer(b)) => a.overlaps_with(b),
            (Array(a, n), Array(b, m)) => a.overlaps_with(b) && n == m,
            (t, Not(n)) | (Not(n), t) => !t.overlaps_with(n),
            (Atom(a), Atom(b)) => a == b,
            (ContextCall(ctx1, var1), ContextCall(ctx2, var2)) => ctx1 == ctx2 && var1 == var2,
            _ => false,
        }
    }

    pub fn union_with(self, other: BoundType) -> BoundType {
        if self == other {
            self
        } else if self.is_subtype_of(&other) {
            other
        } else if other.is_subtype_of(&self) {
            self
        } else {
            BoundType::Union(Box::new(self), Box::new(other))
        }
    }

    pub fn intersection_with(self, other: BoundType) -> BoundType {
        if self == other {
            self
        } else if self.is_subtype_of(&other) {
            self
        } else if other.is_subtype_of(&self) {
            other
        } else {
            BoundType::Intersection(Box::new(self), Box::new(other))
        }
    }
}

#[derive(Clone, Debug)]
pub struct BoundTypingRule {
    pub name: String,
    pub premises: Vec<BoundPremise>,
    pub conclusion: BoundConclusion,
}

#[derive(Debug, Clone)]
pub struct BoundPremise {
    pub setting: Option<BoundTypeSetting>,
    pub judgment: Option<BoundTypingJudgment>,
}

#[derive(Debug, Clone)]
pub struct BoundTypeSetting {
    pub name: String,
    pub extensions: Vec<BoundTypeAscription>,
}

#[derive(Debug, Clone)]
pub struct BoundTypeAscription {
    pub node: Option<NonTerminal>,
    pub ty: BoundType,
}

#[derive(Debug, Clone)]
pub enum BoundTypingJudgment {
    Ascription(BoundTypeAscription),
    Membership(Option<NonTerminal>, String),
}

#[derive(Debug, Clone, Default)]
pub struct BoundConclusionContext {
    pub input: String,
    pub output: Option<BoundTypeSetting>,
}

#[derive(Debug, Clone)]
pub enum BoundConclusionKind {
    Type(BoundType),
    ContextLookup(String, Option<NonTerminal>),
}

#[derive(Debug, Clone)]
pub struct BoundConclusion {
    pub context: BoundConclusionContext,
    pub kind: BoundConclusionKind,
}
