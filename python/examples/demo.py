#!/usr/bin/env python3
"""
Proposition 7 Demo - Type-aware Constrained LLM Generation

Unlike CFG-only approaches, P7 enforces typing rules during generation,
producing not just syntactically valid but also well-typed output.

Supported models:
    - gpt2 (default, small, fast)
    - PleIAs/Monad-1.5B (reasoning model)
    - PleIAs/Baguettotron-3B-instruct (French/English)
    - TinyLlama/TinyLlama-1.1B-Chat-v1.0

Usage:
    python demo.py
    python demo.py --model PleIAs/Monad-1.5B
    python demo.py --example stlc
    python demo.py --interactive
"""

from __future__ import annotations

import argparse
from typing import Optional


# Pre-configured examples with good prompts (ASCII-friendly)
EXAMPLES = {
    "stlc": {
        "grammar": "stlc",
        "description": "Simply Typed Lambda Calculus - generates well-typed lambda terms",
        "prompts": [
            "",                    # Start fresh
            "fn",                  # Start a lambda
            "fn x : Int =>",       # Partial lambda
        ],
    },
    "xtlc": {
        "grammar": "xtlc",
        "description": "Extended STLC with let bindings for context extension",
        "prompts": [
            "",
            "let x : Int",         # Declare x  
            "fn f : Int -> Bool =>",
        ],
    },
    "clike": {
        "grammar": "clike",
        "description": "C-like language with type-checked statements",
        "prompts": [
            "",
            "int",                 # Start variable declaration
            "int x =",
        ],
    },
    "typed_arithmetic": {
        "grammar": "typed_arithmetic",
        "description": "Typed arithmetic with Int/Float/Bool",
        "prompts": [
            "",
            "let x : Int =",
            "let y : Float =",
        ],
    },
}


# Model presets
MODELS = {
    "gpt2": {
        "name": "gpt2",
        "description": "GPT-2 small (124M) - fast, good for testing",
    },
    "monad": {
        "name": "PleIAs/Monad-1.5B",
        "description": "PleIAs Monad 1.5B - reasoning optimized",
    },
    "baguettotron": {
        "name": "PleIAs/Baguettotron-3B-instruct",
        "description": "PleIAs Baguettotron 3B - French/English",
    },
    "tinyllama": {
        "name": "TinyLlama/TinyLlama-1.1B-Chat-v1.0",
        "description": "TinyLlama 1.1B - small and capable",
    },
}


def run_example(gen, example_name: str, num_samples: int = 3) -> None:
    """Run a pre-configured example."""
    example = EXAMPLES[example_name]
    
    print(f"\n{'='*60}")
    print(f"Example: {example_name}")
    print(f"  {example['description']}")
    print(f"{'='*60}\n")
    
    for prompt in example["prompts"]:
        prompt_display = repr(prompt) if prompt else "(empty)"
        print(f"Prompt: {prompt_display}")
        print("-" * 40)
        
        for i in range(num_samples):
            print(f"  {i+1}. ", end="")
            try:
                result = gen.generate(prompt=prompt, stream=True, max_tokens=25)
            except Exception as e:
                print(f"[Error: {e}]")
            print()
        print()


def interactive_mode(gen) -> None:
    """Interactive generation mode."""
    import p7_constrained as p7
    
    print("\n" + "="*60)
    print("Interactive Mode")
    print("  Type a prompt and press Enter to generate")
    print("  Commands: /grammar <name>, /mode <mode>, /quit")
    print("="*60 + "\n")
    
    current_grammar = "stlc"
    current_mode = p7.SamplingMode.CONSTRAINED
    
    while True:
        try:
            user_input = input(f"[{current_grammar}] > ").strip()
        except (EOFError, KeyboardInterrupt):
            print("\nBye!")
            break
        
        if not user_input:
            continue
        
        # Commands
        if user_input.startswith("/"):
            parts = user_input.split()
            cmd = parts[0]
            
            if cmd == "/quit":
                print("Bye!")
                break
            elif cmd == "/grammar" and len(parts) > 1:
                new_grammar = parts[1]
                if new_grammar in p7.list_grammars():
                    current_grammar = new_grammar
                    gen = p7.Generator(gen.model_name, grammar=p7.GRAMMARS[current_grammar])
                    print(f"Switched to grammar: {current_grammar}")
                else:
                    print(f"Unknown grammar. Available: {p7.list_grammars()}")
            elif cmd == "/mode" and len(parts) > 1:
                mode_name = parts[1]
                if mode_name in ["constrained", "bimodal", "free"]:
                    current_mode = {
                        "constrained": p7.SamplingMode.CONSTRAINED,
                        "bimodal": p7.SamplingMode.BIMODAL,
                        "free": p7.SamplingMode.FREE,
                    }[mode_name]
                    print(f"Mode: {mode_name}")
                else:
                    print("Modes: constrained, bimodal, free")
            elif cmd == "/help":
                print("Commands:")
                print("  /grammar <name>  - Switch grammar")
                print("  /mode <mode>     - Switch mode (constrained/bimodal/free)")
                print("  /quit            - Exit")
            else:
                print("Unknown command. Try /help")
            continue
        
        # Generate
        print("→ ", end="")
        try:
            result = gen.generate(prompt=user_input, stream=True, mode=current_mode)
        except Exception as e:
            print(f"[Error: {e}]")
        print()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Proposition 7 Demo - Type-aware Constrained Generation",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python demo.py                           # Quick demo with GPT-2
  python demo.py --model monad             # Use PleIAs Monad
  python demo.py --example stlc            # STLC examples
  python demo.py --example clike           # C-like examples
  python demo.py --interactive             # Interactive mode
  python demo.py --all                     # Run all examples
        """
    )
    parser.add_argument(
        "--model", "-m",
        default="gpt2",
        help="Model: gpt2, monad, baguettotron, tinyllama, or HF path"
    )
    parser.add_argument(
        "--example", "-e",
        choices=list(EXAMPLES.keys()),
        help="Run specific example"
    )
    parser.add_argument(
        "--all", "-a",
        action="store_true",
        help="Run all examples"
    )
    parser.add_argument(
        "--interactive", "-i",
        action="store_true",
        help="Interactive mode"
    )
    parser.add_argument(
        "--samples", "-n",
        type=int,
        default=2,
        help="Samples per prompt"
    )
    parser.add_argument(
        "--temperature", "-t",
        type=float,
        default=0.8
    )
    args = parser.parse_args()
    
    import proposition_7 as p7
    
    # Resolve model name
    model_name = MODELS.get(args.model, {}).get("name", args.model)
    model_desc = MODELS.get(args.model, {}).get("description", args.model)
    
    print(f"""
╔══════════════════════════════════════════════════════════════╗
║                     PROPOSITION 7                            ║
║         Type-aware Constrained LLM Generation                ║
╠══════════════════════════════════════════════════════════════╣
║  Model: {model_desc:<52} ║
║  Grammars: {', '.join(p7.list_grammars()):<50} ║
╚══════════════════════════════════════════════════════════════╝
""")
    
    # Default grammar
    default_grammar = args.example or "stlc"
    
    # Create generator
    config = p7.SamplerConfig(
        temperature=args.temperature,
        max_tokens=30,
    )
    gen = p7.Generator(model_name, grammar=p7.GRAMMARS[default_grammar], config=config)
    
    if args.interactive:
        interactive_mode(gen)
    elif args.all:
        for example_name in EXAMPLES:
            gen = p7.Generator(model_name, grammar=p7.GRAMMARS[example_name], config=config)
            run_example(gen, example_name, num_samples=args.samples)
    elif args.example:
        run_example(gen, args.example, num_samples=args.samples)
    else:
        # Quick demo
        print("Quick demo with STLC (Simply Typed Lambda Calculus):\n")
        print("Generating well-typed lambda terms...\n")
        
        for i in range(3):
            print(f"  {i+1}. ", end="")
            try:
                result = gen.generate(prompt="", stream=True)
            except Exception as e:
                print(f"[Error: {e}]")
            print()
        
        print("\n" + "-"*60)
        print("Try: python demo.py --example clike")
        print("     python demo.py --interactive")
        print("     python demo.py --model monad --all")


if __name__ == "__main__":
    main()
