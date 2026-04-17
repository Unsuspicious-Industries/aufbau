//! Parsing for the type language used inside grammar and rule specifications.

pub use crate::logic::typing::Type;

mod imp {
    include!("../typing/syntax.rs");
}
