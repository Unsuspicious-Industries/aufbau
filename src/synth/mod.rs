pub mod verification;
use crate::ast::ast::FusionAST;
use crate::debug_trace;
use crate::grammar::SPG;
use crate::parse::TypedParser;
use crate::semantics::TypingRuntime;
use crate::typing::{Context, TypingDomain};

#[cfg(test)]
mod tests;

pub struct Synthesizer {
    spg: SPG,
    runtime: TypingRuntime,
    parser: TypedParser,

    input: String,
    ctx: Context,
    tree: Option<FusionAST>,
}

impl Synthesizer {
    pub fn new(spg: SPG, input: impl Into<String>) -> Self {
        let input = input.into();
        debug_trace!("synth", "new: input='{}'", input);
        let runtime = TypingRuntime::new(TypingDomain::default(), spg.clone());
        let parser = TypedParser::new(spg.clone(), runtime.clone());

        Self {
            spg,
            runtime,
            parser,
            ctx: Context::new(),
            input,
            tree: None,
        }
    }

    pub fn grammar(&self) -> &SPG {
        &self.spg
    }

    pub fn runtime(&self) -> &TypingRuntime {
        &self.runtime
    }

    /// Re-run the current input with IR execution recorded, and render the
    /// trace: every instruction, the value it produced or why it could not, the
    /// premise-local scope depth, and each descent into a child.
    ///
    /// Empty without `--features trace`. Discards the cached tree, since the
    /// point is to observe the run rather than reuse its result.
    pub fn explain(&mut self) -> String {
        // The buffer is shared with the parser's clone of the domain, so
        // recording here observes the parse the parser actually runs.
        let trace = self.runtime.trace().clone();
        trace.clear();
        self.tree = None;
        let _ = self.ast();
        trace.render()
    }

    pub fn ctx(&self) -> &Context {
        &self.ctx
    }

    /// Replace the whole context. This is the one authoritative context: every
    /// operation reads `self.ctx`, so a caller must not keep a second copy and
    /// pass it back in.
    ///
    /// Replacement is atomic in the only sense the engine can offer — a single
    /// assignment. Building the `Context` is where a bad type is rejected, so a
    /// caller that fails to parse one never reaches here and the old context
    /// stands. The cached tree is dropped so the next operation observes the
    /// new bindings immediately.
    ///
    /// An identical context is not a change, and re-parsing on every call is
    /// what made the tree cache useless on the hot path, so it is left alone.
    pub fn set_context(&mut self, ctx: Context) {
        if self.ctx != ctx {
            self.ctx = ctx;
            self.tree = None;
        }
    }

    pub fn with_ctx(&mut self, ctx: Context) {
        self.set_context(ctx);
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn set_input(&mut self, input: impl Into<String>) {
        self.input = input.into();
        self.tree = None;
        let _ = self.ast();
    }

    pub fn ast(&mut self) -> Result<FusionAST, String> {
        if let Some(ast) = &self.tree {
            Ok(ast.clone())
        } else {
            let ctx_id = self.runtime.intern_context(self.ctx.clone());
            match self.parser.parse(&self.input, ctx_id) {
                Ok(ast) => {
                    debug_trace!("synth", "ast: input='{}' parsed successfully", self.input);
                    self.tree = Some(ast.clone());
                    Ok(ast)
                }
                Err(err) => {
                    debug_trace!("synth", "ast: input='{}' parse failed: {}", self.input, err);
                    Err(format!("Parse error: {err}"))
                }
            }
        }
    }

    pub fn parse_with(&mut self, ctx: &Context) -> Result<FusionAST, String> {
        self.with_ctx(ctx.clone());
        self.ast()
    }

    pub fn feed_with(&mut self, token: &str, ctx: &Context) -> Result<FusionAST, String> {
        self.with_ctx(ctx.clone());
        self.feed(token)
    }

    /// Extend the input by `token`, committing only if it parses.
    ///
    /// Transactional: the candidate is parsed on a fork, and `input`, the tree
    /// and the parser move forward together or not at all. Installing the input
    /// first and parsing afterwards left a rejected token in `input` with the
    /// tree dropped, so a failed feed silently poisoned every later call.
    pub fn feed(&mut self, token: &str) -> Result<FusionAST, String> {
        debug_trace!("synth", "feed: input='{}' token='{}'", self.input, token);
        let extended = format!("{}{}", self.input, token);
        let mut p = self.parser.fork();
        let ctx_id = self.runtime.intern_context(self.ctx.clone());
        match p.parse(&extended, ctx_id) {
            Ok(ast) => {
                self.parser = p;
                self.input = extended;
                self.tree = Some(ast.clone());
                Ok(ast)
            }
            Err(err) => {
                debug_trace!("synth", "feed rejected '{}': {}", token, err);
                Err(format!("Parse error: {err}"))
            }
        }
    }

    /// Whether `token` would be accepted, changing nothing. A candidate accepted
    /// here is accepted by [`Synthesizer::feed`] from the same state: both parse
    /// the same extended input under the same context.
    #[must_use = "discarding try_feed result hides parse failures"]
    pub fn try_feed(&mut self, token: &str) -> Result<FusionAST, String> {
        debug_trace!("synth", "try: input='{}' token='{}'", self.input, token);
        let extended = format!("{}{}", self.input, token);
        let mut p = self.parser.fork();
        let ctx_id = self.runtime.intern_context(self.ctx.clone());
        match p.parse(&extended, ctx_id) {
            Ok(ast) => Ok(ast),
            Err(err) => Err(format!("try_feed failed: {err}")),
        }
    }
}
