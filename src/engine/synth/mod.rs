use crate::debug_trace;
use crate::engine::grammar::SPG;
use crate::engine::parse::{CtxId, TypedParser};
use crate::engine::structure::ast::FusionAST;
use crate::semantics::domain::ConstraintDomain;
use crate::semantics::runtime::DomainRuntime;

#[cfg(test)]
mod tests;

pub struct Synthesizer<D: ConstraintDomain + Clone> {
    spg: SPG<D>,
    runtime: DomainRuntime<D>,
    parser: TypedParser<D, DomainRuntime<D>>,

    input: String,
    ctx: D::Context,
    tree: Option<FusionAST<D>>,
}

impl<D: ConstraintDomain + Clone + Default> Synthesizer<D> {
    pub fn new(spg: SPG<D>, input: impl Into<String>) -> Self {
        Self::with_domain(D::default(), spg, input)
    }
}

impl<D: ConstraintDomain + Clone> Synthesizer<D> {
    pub fn with_domain(domain: D, spg: SPG<D>, input: impl Into<String>) -> Self {
        let input = input.into();
        debug_trace!("synth", "new: input='{}'", input);
        let ctx = domain.empty_context();
        let runtime = DomainRuntime::new(domain, spg.clone());
        let parser = TypedParser::new(spg.clone(), runtime.clone());

        Self {
            spg,
            runtime,
            parser,
            ctx,
            input,
            tree: None,
        }
    }

    pub fn grammar(&self) -> &SPG<D> {
        &self.spg
    }

    pub fn runtime(&self) -> &DomainRuntime<D> {
        &self.runtime
    }

    pub fn ctx(&self) -> &D::Context {
        &self.ctx
    }

    pub fn with_ctx(&mut self, ctx: D::Context) {
        self.ctx = ctx;
        self.tree = None;
        let _ = self.ast();
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn set_input(&mut self, input: impl Into<String>) {
        self.input = input.into();
        self.tree = None;
        let _ = self.ast();
    }

    pub fn ast(&mut self) -> Result<FusionAST<D>, String> {
        if let Some(ast) = &self.tree {
            Ok(ast.clone())
        } else {
            let ctx_id = ctx_id(&self.ctx, &self.runtime);
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

    pub fn parse_with(&mut self, ctx: &D::Context) -> Result<FusionAST<D>, String> {
        self.with_ctx(ctx.clone());
        self.ast()
    }

    pub fn feed_with(&mut self, token: &str, ctx: &D::Context) -> Result<FusionAST<D>, String> {
        self.with_ctx(ctx.clone());
        self.feed(token)
    }

    pub fn feed(&mut self, token: &str) -> Result<FusionAST<D>, String> {
        debug_trace!("synth", "feed: input='{}' token='{}'", self.input, token);
        let extended = format!("{}{}", self.input, token);
        self.set_input(extended);
        self.ast()
    }

    #[must_use = "discarding try_feed result hides parse failures"]
    pub fn try_feed(&mut self, token: &str) -> Result<FusionAST<D>, String> {
        debug_trace!("synth", "try: input='{}' token='{}'", self.input, token);
        let extended = format!("{}{}", self.input, token);
        let mut p = self.parser.fork();
        match p.parse(&extended, ctx_id(&self.ctx, &self.runtime)) {
            Ok(ast) => Ok(ast),
            Err(err) => Err(format!("try_feed failed: {err}")),
        }
    }
}

fn ctx_id<D: ConstraintDomain>(ctx: &D::Context, runtime: &DomainRuntime<D>) -> CtxId {
    runtime.intern_context(ctx.clone())
}
