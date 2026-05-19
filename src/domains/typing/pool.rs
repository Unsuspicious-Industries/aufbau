use crate::domains::typing::Type;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

pub type SharedType = Arc<Type>;

fn type_pool() -> &'static Mutex<HashMap<Type, SharedType>> {
    static POOL: OnceLock<Mutex<HashMap<Type, SharedType>>> = OnceLock::new();
    POOL.get_or_init(|| Mutex::new(HashMap::new()))
}

pub fn intern_type(ty: Type) -> SharedType {
    let mut pool = type_pool().lock().expect("type pool poisoned");
    if let Some(shared) = pool.get(&ty) {
        return Arc::clone(shared);
    }
    let shared = Arc::new(ty.clone());
    pool.insert(ty, Arc::clone(&shared));
    shared
}
