# proposition-7

**Truth by Construction: From Natural Language to Logic Machines Through Formal Constraints.**

Python bindings for the Proposition 7 constrained generation system that unifies typed programming, proof building, reasoning and natural language.

## Philosophy

Recent developments in the training of Large Language Models (LLMs) have seen the rise of *prover* models. These models are highly optimized for logic and proof through Chain-of-Thought (CoT) enabled Reinforcement Learning on hard math problems. While this approach has gathered good results, it has several fundamental flaws that decrease both accuracy and efficiency.

Under the current training paradigm, LLMs have to internalize three very different notions at the same time:

- **Grammar**
- **Logic**
- **Meaning/Purpose/Creativity**

This structure makes the whole pipeline very hard to debug. Failures on benchmarks and evaluations can come from either syntax errors, logical inconsistencies (what Wittgenstein would call *unsinnig*), or outright limitations of the model's "intelligence".

Some companies, like **.txt** (dottxt.co) are developing regex-based sampling solutions to fix the grammar aspect, in order to improve tool integration and augment reliability. This has proven successful, but it doesn't go far enough — it only covers inference, doesn't integrate in the training pipeline, and doesn't solve the main problem of **logic**.

### The P7 Approach

With constrained generation we want to restrict search space to *formally valid* outputs in a given formal system, and use the broad priors of LLMs as approximations of **taste**, allowing them to pick which path to explore.

In such a setup, "hallucination" loses its meaning: truth is enforced by the formal layer, while open-ended association remains contained in latent representations, or alternative channels.

**A Paradox:**
- Constraints create freedom: removing correctness burden frees the model to explore style, and allows for more creative outputs
- Form enables expression: like meter in poetry, rules focus creativity
- Interesting-ness (or **taste**) resists definition but emerges from examples and use

## System Overview

- **Constraint Engine**: unified grammar and typing/proof rules that produce only valid moves/ASTs within a formal system
- **Syntax independence**: the formally constrained sampler is *syntax independent* and works over runtime-specified context grammars with attached typing rules
- **Constrained pretrained LLM**: ranks valid moves, expressed as tokens

The engine is comprised of three parts:

1. A **meta-engine** that interprets rules to parse code into partial ASTs
2. Rule files, written in a custom DSL inspired by EBNF notation for grammars, and natural deduction style for typing rules
3. A **completion engine** that unifies partial ASTs with rule conclusions to produce valid next tokens

**Generation loop:**

1. Parse input into a Partial AST, with open branches on right-completeable nodes
2. Unify with rule conclusions (up to declared conversion)
3. Silently drop branch rules whose instantiated premises fail
4. Emit admissible completions. Invalid states never materialize.
5. Pass to the LLM to rank the valid moves (token sequences) according to learned taste.

## Installation

### Using pip (from source)

```bash
pip install maturin
cd python/
maturin develop
```

### Using Nix

```bash
nix develop
maturin develop
```

## Quick Start

```python
import p7_constrained as p7

# Create a generator with model + grammar (like Outlines!)
gen = p7.Generator("gpt2", grammar="""
    Number ::= /[0-9]+/
    Op ::= '+' | '-'
    Expr ::= Number | Number Op Expr
    start ::= Expr
""")

# Generate!
result = gen("Generate an arithmetic expression:")
print(result)  # e.g., "42 + 10 - 5"

# Or with more control
result = gen.generate(
    prompt="Calculate:",
    max_tokens=30,
    temperature=0.7,
    mode=p7.SamplingMode.BIMODAL,  # Switch between constrained/free
    stream=True,  # Print tokens as generated
)
```

### Using Built-in Grammars

All built-in grammars include **typing rules** - this is what distinguishes P7 from CFG-only approaches.

```python
import proposition_7 as p7

# List available typed grammars
print(p7.list_grammars())  # ['stlc', 'imp', 'fun']

# FUN: expression-level constrained generation
fun_engine = p7.CompletionEngine(p7.get_grammar("fun"))
fun_engine.feed("let x: Int = 1; x +")
print(fun_engine.debug_completions()["examples"][:5])

# IMP: statement/program constrained generation
imp_engine = p7.CompletionEngine(p7.get_grammar("imp"))
imp_engine.feed("x: Int = 1; if x < 5 { y: Int = x + 1; } else { y: Int =")
print(imp_engine.debug_completions()["examples"][:5])

# Available grammars:
# - stlc: simply typed lambda calculus
# - fun: typed functional language (let/lambda/application)
# - imp: typed imperative language (assign/if/while)
```

### Low-level API

```python
import p7_constrained as p7

# Direct grammar + constraint engine access
grammar = p7.Grammar("""...""")
gen = p7.ConstrainedGenerator(grammar)

gen.feed("42")
print(gen.is_valid_next("+"))  # True
print(gen.get_valid_patterns())  # Regex patterns for valid tokens
print(gen.is_complete())  # Check if parse is complete
```

## Grammar Format

The p7 grammar format supports:

- **Productions**: `Name ::= symbol1 symbol2 | alternative`
- **Literals**: `'hello'` or `"world"`
- **Regex patterns**: `/[a-z]+/`
- **Bindings**: `Expr[x]` to name subexpressions
- **Typing rules**: Inference-style rules for type checking

### Example: Simply Typed Lambda Calculus

```
Identifier ::= /[a-z][a-zA-Z0-9]*/
Variable(dec) ::= Identifier[x]
Lambda(lambda) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]
Application(app) ::= Expression[e1] Expression[e2]

BaseType ::= Identifier[τ]
FunctionType ::= Type[τ1] '->' Type[τ2]
Type ::= BaseType | FunctionType | '(' Type ')'

Expression ::= Variable | Lambda | Application | '(' Expression ')'

x ∈ Γ
----------- (dec)
Γ ⊢ Γ(x)

Γ[x:τ1] ⊢ e : τ2
----------------------- (lambda)
Γ ⊢ τ1 → τ2

Γ ⊢ e1 : τ1 → τ2,   Γ ⊢ e2 : τ1
-------------------------------- (app)
Γ ⊢ τ2
```

## HuggingFace Transformers Integration

The `Generator` class handles HuggingFace integration automatically:

```python
import p7_constrained as p7

# GPT-2 (default, small)
gen = p7.Generator("gpt2", grammar=p7.GRAMMARS["arithmetic"])

# TinyLlama
gen = p7.Generator("TinyLlama/TinyLlama-1.1B-Chat-v1.0", grammar=p7.GRAMMARS["json"])

# Any causal LM
gen = p7.Generator("mistralai/Mistral-7B-v0.1", grammar=my_grammar)

# Generate
result = gen("Generate something:")
```

### Sampling Modes

```python
import p7_constrained as p7

gen = p7.Generator("gpt2", grammar=my_grammar)

# Fully constrained (default) - only valid tokens
result = gen.generate(mode=p7.SamplingMode.CONSTRAINED)

# Free generation - no constraints
result = gen.generate(mode=p7.SamplingMode.FREE)

# Bimodal - switches to free mode when stuck (high entropy)
# then returns to constrained mode
result = gen.generate(mode=p7.SamplingMode.BIMODAL)
```

### Configuration

```python
import p7_constrained as p7

config = p7.SamplerConfig(
    temperature=0.8,
    top_k=50,
    top_p=0.95,
    max_tokens=100,
    mode=p7.SamplingMode.BIMODAL,
    entropy_threshold=4.0,  # Switch to free if entropy > this
    free_token_limit=10,    # Max free tokens before switching back
)

gen = p7.Generator("gpt2", grammar=my_grammar, config=config)
```

## API Reference

### `Grammar`

```python
grammar = p7.Grammar(spec_string)
```

Create a grammar from a spec string.

### `ConstrainedGenerator`

```python
gen = p7.ConstrainedGenerator(grammar)
gen.reset()                          # Reset state
gen.feed(token)                      # Feed a token (with space)
gen.feed_raw(text)                   # Feed raw text
gen.is_complete()                    # Check if parse is complete
gen.get_valid_patterns()             # Get valid token patterns
gen.is_valid_next(token)             # Check if token is valid
gen.get_token_mask(vocab)            # Get boolean mask for vocabulary
gen.get_valid_token_indices(vocab)   # Get indices of valid tokens
```

### `ConstrainedLogitsProcessor`

```python
proc = p7.ConstrainedLogitsProcessor(grammar, eos_token_id=None)
proc.init_vocab(token_strings)       # Initialize vocabulary mapping
proc.reset()                         # Reset state
proc.feed_token(token_str)           # Feed a token
proc.get_allowed_tokens()            # Get allowed token indices
proc.is_complete()                   # Check completion
```

### Utility Functions

```python
p7.regex_matches(pattern, text)       # Check if text matches regex
p7.regex_prefix_valid(pattern, prefix)  # Check if prefix is valid
```

## Practical Takeaways

- **Practitioners**: Use constraints to eliminate invalid states; devote model capacity to style and strategy.
- **Decision-makers**: Expect better efficiency (less wasted compute) and better interpretability and traceability of the process.
- **Researchers**: Treat interesting-ness as a learnable value function over valid moves; compare signals (structural, graph, human preference).

## Related Work

This approach draws inspiration from:

- [Type-Constrained Code Generation with Language Models](https://arxiv.org/abs/2504.09246) (Mündler et al., 2025) — type-aware constraint generation for LLM code synthesis
- Attribute grammars (Knuth, 1968) and bidirectional type checking (Pierce & Turner, 2000)
- Martin-Löf's Intuitionistic Type Theory
- Wittgenstein's *Tractatus Logico-Philosophicus*

| Feature | proposition-7 | Type-Constrained (arXiv:2504.09246) | Outlines/.txt |
|---------|----------------|-------------------------------------|---------------|
| Type-aware | ✅ | ✅ | ❌ |
| Custom languages | ✅ Any `.spec` grammar | ❌ TypeScript/STLC only | ❌ |
| Declarative rules | ✅ Inference-style | ❌ Hardcoded | ❌ |
| Regex tokens | ✅ | Partial | ✅ |
| Persistent state | ✅ | ✅ | ❌ |

## References

### Philosophy & Foundations
- Wittgenstein, L. (1921). *Tractatus Logico-Philosophicus*.
- Kant, I. (1790). *Critique of Judgment*.

### Core Type Theory
- Martin-Löf, P. (1984). Intuitionistic Type Theory.
- Girard, J.-Y. (1972). System F / cut elimination.
- Barendregt, H. (1992). Lambda calculi with types.
- Wadler, P. (2015). Propositions as Types.

### Constrained Generation & Synthesis
- Muendler, N., et al. (2025). Type-aware constraint generation for LLM code synthesis.
- Willard, B., & Louf, R. (2023). Efficient guided generation for large language models.
- Polikarpova, Kuraj, & Solar-Lezama (2016). Refinement type synthesis.
- Knuth, D. E. (1968). Semantics of context-free languages.
- Pierce, B. C., & Turner, D. N. (2000). Local type inference.

## License

MIT

---

*The project is named after proposition 7 in Wittgenstein's Tractatus: "Whereof one cannot speak, thereof one must be silent" (Wovon man nicht sprechen kann, darüber muß man schweigen). Our system aims to clearly delineate what can be formally expressed versus what belongs to the realm of the unsayable.*
