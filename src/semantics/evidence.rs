use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use crate::engine::parse::arena::{EvidenceId, BOT, TOP};

pub struct EvidenceStore<E: Hash + Eq + Clone> {
    values: Rc<RefCell<Vec<E>>>,
    index: Rc<RefCell<HashMap<E, EvidenceId>>>,
}

impl<E: Hash + Eq + Clone> Clone for EvidenceStore<E> {
    fn clone(&self) -> Self {
        Self {
            values: Rc::clone(&self.values),
            index: Rc::clone(&self.index),
        }
    }
}

impl<E: Hash + Eq + Clone> EvidenceStore<E> {
    pub fn new(top: E, bottom: E) -> Self {
        let store = Self {
            values: Rc::new(RefCell::new(Vec::new())),
            index: Rc::new(RefCell::new(HashMap::new())),
        };
        let top_id = store.intern(top);
        debug_assert_eq!(top_id, TOP, "top evidence must intern at TOP=0");
        let bot_id = store.intern(bottom);
        debug_assert_eq!(bot_id, BOT, "bottom evidence must intern at BOT=1");
        store
    }

    pub fn intern(&self, value: E) -> EvidenceId {
        if let Some(&id) = self.index.borrow().get(&value) {
            return id;
        }
        let mut values = self.values.borrow_mut();
        let id = values.len();
        values.push(value.clone());
        drop(values);
        self.index.borrow_mut().insert(value, id);
        id
    }

    #[must_use]
    pub fn get(&self, id: EvidenceId) -> Option<E> {
        self.values.borrow().get(id).cloned()
    }
}
