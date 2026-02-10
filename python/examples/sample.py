#!/usr/bin/env python3
"""TypedSampler example with randomized char-level sampling."""

import argparse

import random
import time
import proposition_7 as p7

VOCAB = list(
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789"
    " \n"
    "λ"
    "->"
    ".:;,(){}[]"
    "+-*/=<>!|"
)

PRESETS = {
    "stlc": "λf:(Int->Bool).λx:Int.",
    "fun": "let x: Int = 1; x +",
    "imp": "x: Int = 1; if x < 3 { y: Int = x +",
}

class Timer:
    def __init__(self):
        self.times = {}
        self.counts = {}
    
    def __call__(self, name: str):
        return TimerContext(self, name)
    
    def record(self, name: str, elapsed: float):
        if name not in self.times:
            self.times[name] = 0.0
            self.counts[name] = 0
        self.times[name] += elapsed
        self.counts[name] += 1
    
    def report(self):
        print("\n--- Profiling ---")
        total = sum(self.times.values())
        for name in sorted(self.times.keys(), key=lambda n: -self.times[n]):
            t = self.times[name]
            c = self.counts[name]
            pct = (t / total * 100) if total > 0 else 0
            avg = (t / c * 1000) if c > 0 else 0
            print(f"  {name:20s}: {t:7.3f}s ({pct:5.1f}%) | {c:4d} calls | {avg:.2f}ms/call")
        print(f"  {'TOTAL':20s}: {total:7.3f}s")


class TimerContext:
    def __init__(self, timer: Timer, name: str):
        self.timer = timer
        self.name = name
    
    def __enter__(self):
        self.start = time.perf_counter()
        return self
    
    def __exit__(self, *args):
        elapsed = time.perf_counter() - self.start
        self.timer.record(self.name, elapsed)


def random_logits() -> list[float]:
    return [random.gauss(0, 2) for _ in VOCAB]


def sample_token(logits: list[float], temperature: float = 1.0) -> int:
    import math
    
    valid = [(i, l) for i, l in enumerate(logits) if l > -1000]
    if not valid:
        return -1
    
    scaled = [l / temperature for _, l in valid]
    max_l = max(scaled)
    exps = [math.exp(l - max_l) for l in scaled]
    total = sum(exps)
    probs = [e / total for e in exps]
    
    r = random.random()
    cumsum = 0
    for (idx, _), prob in zip(valid, probs):
        cumsum += prob
        if r < cumsum:
            return idx
    
    return valid[-1][0]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--grammar",
        choices=p7.list_grammars(),
        default="fun",
        help="Built-in grammar to sample with",
    )
    parser.add_argument(
        "--initial",
        default=None,
        help="Initial prefix to feed before generation",
    )
    args = parser.parse_args()

    timer = Timer()

    print("=" * 60)
    print("TypedSampler: Constrainted Character-Level Generation")
    print("=" * 60)
    print(f"\nVocab size: {len(VOCAB)} chars")

    initial = args.initial if args.initial is not None else PRESETS[args.grammar]
    print(f"Grammar: {args.grammar}")

    with timer("init"):
        sampler = p7.TypedSampler(
            grammar=p7.get_grammar(args.grammar),
            vocab=VOCAB,
            logit_fn=random_logits,
        )

    print(f"\n--- Starting with: '{initial}' ---")
    with timer("feed_initial"):
        sampler.feed(initial)
    
    print("\nGenerating tokens (constrainted to well-typed):")
    generated = initial
    
    PRE_TOP_K = 20
    
    for step in range(100):
        with timer("infer"):
            masked_logits = sampler.infer(pre_top_k=PRE_TOP_K)
        
        valid_count = sum(1 for l in masked_logits if l > -1000)
        
        with timer("sample"):
            token_idx = sample_token(masked_logits, temperature=0.8)
        
        if token_idx < 0:
            print(f"\n  Step {step}: No valid tokens! Done.")
            break
        
        token = VOCAB[token_idx]
        
        try:
            with timer("feed"):
                sampler.feed(token)
            generated += token
            if step % 1 == 0:
                print(f"  Step {step:3d}: '{repr(token)[1:-1]}' (valid: {valid_count}/{len(VOCAB)})")
        except TypeError as e:
            print(f"  Step {step:3d}: Rejected '{token}' - {e}")
            break
    
    print(f"\n--- Final ---")
    print(f"Generated ({len(generated)} chars): '{generated}'")
    print(f"Is complete: {sampler.is_complete()}")
    
    print("\n--- Valid tokens rn ---")
    with timer("infer_text"):
        top_k = sampler.infer_text(k=10, pre_top_k=20)
    print(f"Top 10: {top_k}")
    
    print("\n--- S-expr AST ---")
    try:
        with timer("to_sexpr"):
            sexpr = sampler.generator.to_sexpr()
        print(sexpr[:500] + "..." if len(sexpr) > 500 else sexpr)
    except Exception as e:
        print(f"Cannot serialize (incomplete): {e}")
        print("(Parse incomplete)")
    
    timer.report()


if __name__ == "__main__":
    main()
