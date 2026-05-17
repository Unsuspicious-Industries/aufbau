use super::{Item, TypedParser};
use crate::logic::error::PrefixError;
use crate::logic::{Obligations, SemanticRuntime};

#[cfg(test)]
impl<T: SemanticRuntime> TypedParser<T> {
    pub(crate) fn enqueue_process_for_test(&mut self, item: Item) {
        self.enqueue_process(item);
    }

    pub(crate) fn seed_for_test(&mut self, nt: usize, pos: usize, ctx: usize) {
        let prods: Vec<_> = self
            .grammar
            .productions_at(nt)
            .map(|p| (0..p.len()).map(|i| (nt, i)).collect())
            .unwrap_or_default();
        self.seed(&prods, pos, ctx, &Obligations::empty());
    }

    pub(crate) fn process_for_test(&mut self, item: Item) -> Result<(), PrefixError> {
        self.process(item)
    }

    pub(crate) fn complete_for_test(
        &mut self,
        completion: super::parser::Completion,
    ) -> Result<(), PrefixError> {
        self.complete(completion)
    }
}
