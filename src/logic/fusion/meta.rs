use super::{CtxId, TypedParser, TypedPrefixError, TypedPrefixState, TypingRuntime};
use crate::logic::parse::arena::ParseArena;

#[derive(Debug)]
pub struct MetaTypedParser<T> {
    parser: TypedParser<T>,
    start_depth: u16,
    max_depth: u16,
    depth_factor: f64,
}

impl<T> Clone for MetaTypedParser<T>
where
    T: TypingRuntime + Clone,
{
    fn clone(&self) -> Self {
        Self {
            parser: self.parser.fork(),
            start_depth: self.start_depth,
            max_depth: self.max_depth,
            depth_factor: self.depth_factor,
        }
    }
}

impl<T> MetaTypedParser<T>
where
    T: TypingRuntime + Clone,
{
    pub fn new(parser: TypedParser<T>) -> Self {
        Self {
            parser,
            start_depth: 4,
            max_depth: 128,
            depth_factor: 1.5,
        }
    }

    pub fn with_start_depth(mut self, start_depth: u16) -> Self {
        self.start_depth = start_depth.max(1);
        self
    }

    pub fn with_max_depth(mut self, max_depth: u16) -> Self {
        self.max_depth = max_depth.max(self.start_depth);
        self
    }

    pub fn with_depth_factor(mut self, depth_factor: f64) -> Self {
        self.depth_factor = depth_factor.max(1.1);
        self
    }

    pub fn parse(
        &self,
        input: &str,
        ctx: CtxId,
    ) -> Result<(TypedPrefixState, u16), TypedPrefixError> {
        self.parse_with_arena(input, ctx)
            .map(|(state, depth, _arena)| (state, depth))
    }

    pub(crate) fn parse_with_arena(
        &self,
        input: &str,
        ctx: CtxId,
    ) -> Result<(TypedPrefixState, u16, ParseArena), TypedPrefixError> {
        let mut depth = self.start_depth;

        loop {
            let mut parser = self.parser.fork().with_max_depth(depth);
            match parser.parse(input, ctx) {
                Ok(state) => return Ok((state, depth, parser.arena().snapshot())),
                Err(err) => {
                    if depth >= self.max_depth {
                        return Err(err);
                    }
                    let mut next = ((depth as f64) * self.depth_factor).ceil() as u16;
                    if next <= depth {
                        next = depth + 1;
                    }
                    depth = next.min(self.max_depth);
                }
            }
        }
    }
}
