use crate::debug_trace;
use crate::logic::grammar::Grammar;
use crate::logic::parse::{CtxId, TypedParser};
use crate::logic::structure::ast::FusionAST;
use crate::logic::typing::Context;

use crate::logic::typing::runtime::RuleRuntime;

#[cfg(test)]
mod tests;

pub struct Synthesizer {
    grammar: Grammar,
    runtime: RuleRuntime,
    parser: TypedParser<RuleRuntime>,

    input: String,
    ctx: Context,
    tree: Option<FusionAST>,
}

impl Synthesizer {
    pub fn new(grammar: Grammar, input: impl Into<String>) -> Self {
        let input = input.into();
        debug_trace!("synth", "new: input='{}'", input);
        let runtime = RuleRuntime::new(grammar.clone());
        let parser = TypedParser::new(grammar.clone(), runtime.clone());

        Self {
            grammar,
            runtime,
            parser,
            ctx: Context::new(),
            input,
            tree: None,
        }
    }

    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    pub fn runtime(&self) -> &RuleRuntime {
        &self.runtime
    }

    pub fn ctx(&self) -> &Context {
        &self.ctx
    }
    pub fn set_ctx(&mut self, ctx: Context) {
        self.ctx = ctx;
        self.tree = None;
        let _ = self.ast(); // reload the AST
    }
    pub fn add_ctx(&mut self, ctx: Context) {
        self.ctx.merge(&ctx);
        self.tree = None;
        let _ = self.ast(); // reload the AST
    }

    pub fn input(&self) -> &str {
        &self.input
    }
    pub fn set_input(&mut self, input: impl Into<String>) {
        self.input = input.into();
        self.tree = None;
        let _ = self.ast(); // reload the AST
    }

    pub fn ast(&mut self) -> Result<FusionAST, String> {
        match &self.tree {
            Some(ast) => Ok(ast.clone()),
            None => {
                match self
                    .parser
                    .parse(&self.input, ctx_id(&self.ctx, &self.runtime))
                {
                    Ok(ast) => {
                        debug_trace!("synth", "ast: input='{}' parsed successfully", self.input);
                        self.tree = Some(ast.clone());
                        return Ok(ast);
                    }
                    Err(err) => {
                        debug_trace!("synth", "ast: input='{}' parse failed: {}", self.input, err);
                        Err(format!("Parse error: {}", err))
                    }
                }
            }
        }
    }

    pub fn parse_with(&mut self, ctx: &Context) -> Result<FusionAST, String> {
        self.set_ctx(ctx.clone());
        self.ast()
    }

    pub fn feed_with(&mut self, token: &str, ctx: &Context) -> Result<FusionAST, String> {
        self.set_ctx(ctx.clone());
        self.feed(token)
    }

    // feed mutates the synthesizer
    pub fn feed(&mut self, token: &str) -> Result<FusionAST, String> {
        debug_trace!("synth", "feed: input='{}' token='{}'", self.input, token);
        let extended =
            crate::logic::grammar::extend::extend_input(&mut self.grammar, &self.input, token);
        // realoding might be improved but not time
        self.set_input(extended);
        self.ast()
    }

    // doesnt mutate
    pub fn try_feed(&mut self, token: &str) -> Result<FusionAST, String> {
        debug_trace!("synth", "try: input='{}' token='{}'", self.input, token);
        let extended =
            crate::logic::grammar::extend::extend_input(&mut self.grammar, &self.input, token);
        let mut p = self.parser.fork();
        match p.parse(&extended, ctx_id(&self.ctx, &self.runtime)) {
            Ok(ast) => Ok(ast),
            Err(err) => Err(format!("try_feed failed: {}", err)),
        }
    }
}

fn ctx_id(ctx: &Context, runtime: &RuleRuntime) -> CtxId {
    runtime.intern_context(ctx.clone())
}
