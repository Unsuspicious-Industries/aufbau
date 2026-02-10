#!/usr/bin/env python3
"""CompletionEngine smoke checks for built-in fun/imp constrained decoding."""

import proposition_7 as p7


CASES = {
    "fun": [
        "(x: Int) => x +",
        "let x: Int = 1; x +",
        "let f: Float = 1.0; f +.",
    ],
    "imp": [
        "x: Int = 1; if x < 5 { y: Int = x + 1; } else { y: Int =",
        "counter: Int = 0; while counter < 3 { counter +",
        "flag: Int|Bool = true; if flag == true { z: Int = 1; } else { z: Int =",
    ],
}


def run_case(grammar_name: str, seed: str) -> None:
    engine = p7.CompletionEngine(p7.get_grammar(grammar_name))
    print(f"[{grammar_name}] {seed}")
    try:
        engine.feed(seed)
        completions = engine.debug_completions()
        sample = completions.get("examples", [])[:10]
        patterns = completions.get("patterns", [])[:5]
        print(f"  completion examples: {sample}")
        print(f"  regex patterns:      {patterns}")
    except TypeError as err:
        print(f"  type error: {err}")
    print()


def main() -> None:
    for grammar_name, seeds in CASES.items():
        print(f"=== {grammar_name.upper()} ===")
        for seed in seeds:
            run_case(grammar_name, seed)


if __name__ == "__main__":
    main()
