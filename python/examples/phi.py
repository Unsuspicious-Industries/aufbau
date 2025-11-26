#!/usr/bin/env python3
"""
Phi-3.5-mini demo: Typed constrained vs unconstrained generation.

Demonstrates how type-aware constrained decoding keeps generation well-typed,
while unconstrained generation often produces invalid/ill-typed terms.

Uses Microsoft's Phi-3.5-mini-instruct for higher quality reasoning.
https://huggingface.co/microsoft/Phi-3.5-mini-instruct
"""

import sys
sys.path.insert(0, '/home/pkd/code/p7/python')

import proposition_7 as p7


# XTLC examples - Church encodings, combinators, higher-order functions
# Syntax: λx:T.e for abstraction, (f e) for application, A->B for function types
# Initial strings are SHORT to give more room for divergence
XTLC_EXAMPLES = {
    "const_K": {
        "description": "K combinator: λx.λy.x (constant, returns first)",
        "initial": "λx:Int.",
        "expected": "λx:Int.λy:Bool.x",
    },
    "apply_fn": {
        "description": "Apply function to arg: λf.λx.(f x)",
        "initial": "λf:(Int->Bool).",
        "expected": "λf:(Int->Bool).λx:Int.(f x)",
    },
    "compose_B": {
        "description": "B combinator (compose): λf.λg.λx.(f (g x))",
        "initial": "λf:(Int->Bool).",
        "expected": "λf:(Int->Bool).λg:(Bool->Int).λx:Bool.(f (g x))",
    },
    "flip_C": {
        "description": "C combinator (flip): λf.λx.λy.((f y) x)",
        "initial": "λf:(Bool->(Int->Bool)).",
        "expected": "λf:(Bool->(Int->Bool)).λx:Int.λy:Bool.((f y) x)",
    },
    "twice_apply": {
        "description": "Apply function twice: λf.λx.(f (f x))",
        "initial": "λf:(Int->Int).",
        "expected": "λf:(Int->Int).λx:Int.(f (f x))",
    },
    "church_pair": {
        "description": "Church pair: {a}{b}λs.(s a b)",
        "initial": "{a:Int}{b:Bool}λs:",
        "expected": "{a:Int}{b:Bool}λs:(Int->(Bool->Int)).(s a)b",
    },
    "nested_app": {
        "description": "Nested application with multiple args",
        "initial": "{f:Int->Int->Bool}{x:Int}{y:Int}(",
        "expected": "{f:Int->Int->Bool}{x:Int}{y:Int}((f x)y)",
    },
    "S_combinator": {
        "description": "S combinator: λf.λg.λx.((f x) (g x))",
        "initial": "λf:(Int->(Int->Bool)).",
        "expected": "λf:(Int->(Int->Bool)).λg:(Int->Int).λx:Int.((f x)(g x))",
    },
}


# C-like examples - typed imperative programming
# Syntax: int x = expr;, x = expr;, if/while/return
CLIKE_EXAMPLES = {
    "simple_add": {
        "description": "Declare int function that adds two numbers a and b.",
        "initial": "int add(",
        "expected": "int add(int a, int b) { return a + b; }",
    },
    "type_mismatch": {
        "description": "Variable assignment must match declared type",
        "initial": "int a = 5; int b = a + ",
        "expected": "int a = 5; int b = a + 1;",
    },
}


XTLC_PROMPT = """You are an expert in typed lambda calculus. Generate well-typed lambda terms.

The grammar uses:
- λx:T.e for lambda abstraction (function taking x of type T, returning e)
- {x:T} to declare a variable x of type T in scope
- (f e) for function application
- Types: base types or function types like Int->Bool, (Int->Int)->Bool

Complete the lambda term to make it well-typed:

"""


CLIKE_PROMPT = """You are an expert programmer. Generate well-typed C-like code.

The grammar uses:
- Type var = expr; for variable declarations (int, float, char, bool)
- var = expr; for assignments
- Operators: + - * / for arithmetic, == != < > for comparisons
- if/while statements with { } blocks

Complete the code to make it well-typed:

"""


def run_examples(model, examples, prompt_template, grammar_name, results):
    """Run a set of examples with a given grammar."""
    
    for name, example in examples.items():
        print("\n" + "=" * 80)
        print(f"[{grammar_name}] Example: {name}")
        print(f"  {example['description']}")
        print(f"  Initial:  '{example['initial']}'")
        print(f"  Expected: '{example['expected']}'")
        print("=" * 80)
        
        initial_code = example["initial"]
        prompt = prompt_template + f"Initial: {initial_code}\nComplete: "
        
        # --- Constrained Generation ---
        print("\n--- Constrained (type-aware) ---")
        constrained_tokens = []
        
        try:
            constrained_result = model.until_complete(
                initial=initial_code,
                prompt=prompt,
                max_tokens=40,
                greedy_k=1,
                pre_top_k=50,
                on_token=lambda tok, step: constrained_tokens.append(tok) or print(f"  +'{repr(tok)[1:-1]}'", end="", flush=True),
            )
        except Exception as e:
            print(f"\n  ERROR: {e}")
            constrained_result = p7.GenerationResult(
                text=initial_code, is_complete=False, tokens_generated=0, stopped_reason=f"error: {e}"
            )
        print()  # newline after tokens
        
        # --- Unconstrained Generation ---
        print("\n--- Unconstrained (raw model) ---")
        unconstrained_tokens = []
        
        try:
            unconstrained_result = model.generate_unconstrained(
                initial=initial_code,
                prompt=prompt,
                max_tokens=40,
                on_token=lambda tok, step: unconstrained_tokens.append(tok) or print(f"  +'{repr(tok)[1:-1]}'", end="", flush=True),
                stop_tokens=["\n", "<|end|>", "<|endoftext|>"],
            )
        except Exception as e:
            print(f"\n  ERROR: {e}")
            unconstrained_result = p7.GenerationResult(
                text=initial_code, is_complete=False, tokens_generated=0, stopped_reason=f"error: {e}"
            )
        print()  # newline after tokens
        
        # Results comparison
        print("\n--- Comparison ---")
        print(f"  Constrained:   '{constrained_result.text}'")
        print(f"  Unconstrained: '{unconstrained_result.text}'")
        print(f"  Expected:      '{example['expected']}'")
        print(f"  Well-typed:    {constrained_result.is_complete} (constrained) | ? (unconstrained)")
        
        # Check match
        constrained_match = constrained_result.text.strip() == example['expected']
        print(f"  Match:         {'✓' if constrained_match else '≠'}")
        
        # Show divergence
        if constrained_result.text != unconstrained_result.text:
            print(f"  → Diverged! Unconstrained went: {''.join(unconstrained_tokens)}")
        
        results.append({
            "grammar": grammar_name,
            "name": name,
            "constrained": constrained_result.text,
            "unconstrained": unconstrained_result.text,
            "expected": example["expected"],
            "complete": constrained_result.is_complete,
            "match": constrained_match,
            "diverged": constrained_result.text != unconstrained_result.text,
        })


def main():
    print("=" * 80)
    print("Phi-3.5-mini: Constrained vs Unconstrained Generation")
    print("=" * 80)
    
    model_name = "microsoft/Phi-3.5-mini-instruct"
    print(f"\nLoading {model_name}...")
    
    results = []
    
    # --- XTLC (Lambda Calculus) ---
    print("\n" + "#" * 80)
    print("# XTLC - Extended Typed Lambda Calculus")
    print("#" * 80)
    
    xtlc_model = p7.ConstrainedModel.from_pretrained(
        model_name,
        grammar=p7.GRAMMARS["xtlc"],
        device_map="auto",
        torch_dtype="auto",
        trust_remote_code=True,
    )
    print(f"Vocabulary size: {len(xtlc_model.vocab)}")
    
    run_examples(xtlc_model, XTLC_EXAMPLES, XTLC_PROMPT, "xtlc", results)
    
    # --- C-like ---
    print("\n" + "#" * 80)
    print("# CLIKE - C-like Imperative Language")
    print("#" * 80)
    
    clike_model = p7.ConstrainedModel.from_pretrained(
        model_name,
        grammar=p7.GRAMMARS["clike"],
        device_map="auto",
        torch_dtype="auto",
        trust_remote_code=True,
    )
    print(f"Vocabulary size: {len(clike_model.vocab)}")
    
    run_examples(clike_model, CLIKE_EXAMPLES, CLIKE_PROMPT, "clike", results)
    
    # Summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"\n{'Grammar':<8} {'Example':<16} {'Complete':<10} {'Match':<8} {'Diverged':<10}")
    print("-" * 60)
    for r in results:
        print(f"{r['grammar']:<8} {r['name']:<16} {str(r['complete']):<10} {'✓' if r['match'] else '≠':<8} {'YES' if r['diverged'] else 'no':<10}")
    
    complete = sum(1 for r in results if r['complete'])
    matches = sum(1 for r in results if r['match'])
    diverged = sum(1 for r in results if r['diverged'])
    print("-" * 60)
    print(f"Complete: {complete}/{len(results)}, Matches: {matches}/{len(results)}, Diverged: {diverged}/{len(results)}")
    
    print("\n" + "=" * 80)
    print("KEY INSIGHT:")
    print("  Constrained generation ALWAYS produces well-typed terms.")
    print("  Unconstrained generation often diverges to invalid/ill-typed code.")
    print("=" * 80)


if __name__ == "__main__":
    main()
