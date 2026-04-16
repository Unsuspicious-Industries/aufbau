use super::{Item, TypedParser};
use crate::logic::fusion::{PrefixError, TypingRuntime};

#[cfg(test)]
impl<T: TypingRuntime> TypedParser<T> {
    pub(crate) fn reset_tables_for_test(&mut self) {
        self.tables = Default::default();
    }

    pub(crate) fn enqueue_process_for_test(&mut self, item: Item) {
        self.enqueue_process(item);
    }

    pub(crate) fn seed_for_test(&mut self, nt: usize, pos: usize, ctx: usize) {
        let prods: Vec<_> = self
            .grammar
            .productions_at(nt)
            .map(|p| (0..p.len()).map(|i| (nt, i)).collect())
            .unwrap_or_default();
        self.seed(&prods, pos, ctx, &[]);
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

    pub(crate) fn consume_for_test(
        &mut self,
        item: &Item,
        regex: &crate::regex::Regex,
        symbol: &crate::logic::grammar::Symbol,
    ) -> Result<Option<Item>, PrefixError> {
        self.consume(item, regex, symbol)
    }
}
