#!/usr/bin/env python3
"""GPT-2 constrained decoding demo for built-in grammars."""

import argparse

import torch
from transformers import GPT2LMHeadModel, GPT2Tokenizer
import proposition_7 as p7


PRESETS = {
    "stlc": {
        "prompt": "Complete this simply typed lambda calculus expression:\n",
        "initial": "λf:(Int->Bool).λx:Int.",
    },
    "fun": {
        "prompt": "Complete this typed functional expression:\n",
        "initial": "let x: Int = 1; x +",
    },
    "imp": {
        "prompt": "Complete this typed imperative program:\n",
        "initial": "x: Int = 1; if x < 3 { y: Int = x +",
    },
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--grammar",
        default="fun",
        choices=p7.list_grammars(),
        help="Built-in grammar to use for constrained decoding",
    )
    parser.add_argument("--prompt", default=None, help="Override prompt text")
    parser.add_argument("--initial", default=None, help="Override initial constrained seed")
    parser.add_argument("--max-tokens", type=int, default=20, help="Maximum generated tokens")
    parser.add_argument("--pre-top-k", type=int, default=100, help="Candidate tokens before filtering")
    parser.add_argument("--greedy-k", type=int, default=1, help="Top-k among valid tokens")
    return parser.parse_args()


def main():
    args = parse_args()
    preset = PRESETS.get(args.grammar, PRESETS["fun"])
    prompt = args.prompt if args.prompt is not None else preset["prompt"]
    initial_code = args.initial if args.initial is not None else preset["initial"]

    print("=" * 60)
    print("GPT-2 + Typed Constrained Generation")
    print("=" * 60)

    print("\nLoading GPT-2...")
    tokenizer = GPT2Tokenizer.from_pretrained("gpt2")
    model = GPT2LMHeadModel.from_pretrained("gpt2")
    model.eval()

    vocab_size = tokenizer.vocab_size
    vocab = [tokenizer.decode([i]) for i in range(vocab_size)]
    print(f"Vocab size: {vocab_size}")
    print(f"Grammar: {args.grammar}")

    input_ids = None

    def get_logits() -> list[float]:
        nonlocal input_ids
        with torch.no_grad():
            outputs = model(input_ids)
            logits = outputs.logits[0, -1, :].tolist()
        return logits

    print(f"Creating sampler with {args.grammar} grammar...")
    sampler = p7.TypedSampler(
        grammar=p7.get_grammar(args.grammar),
        vocab=vocab,
        logit_fn=get_logits,
    )

    print(f"\nPrompt: '{prompt}'")
    print(f"Initial: '{initial_code}'")

    prompt_ids = tokenizer.encode(prompt + initial_code, return_tensors="pt")
    input_ids = prompt_ids

    sampler.feed(initial_code)
    print(f"Sampler state: '{sampler.current_text()}'")
    unconstrained_text = sampler.current_text()

    print("\n--- Generating (constrainted to well-typed) ---")
    for step in range(args.max_tokens):
        next_token = sampler.infer_greedy(k=args.greedy_k, pre_top_k=args.pre_top_k)
        unconstrained_pick = sampler.infer_unconstrained(k=args.greedy_k)

        if next_token is None and unconstrained_pick is None:
            print(f"Step {step}: No tokens, stopping.")
            break

        if next_token is not None:
            try:
                sampler.feed(next_token)
            except TypeError as e:
                print(f"Step {step}: Type error with '{next_token}': {e}")
                break

        if next_token is not None:
            token_id = tokenizer.encode(next_token, add_special_tokens=False)
            if token_id:
                input_ids = torch.cat([input_ids, torch.tensor([token_id])], dim=1)

        if unconstrained_pick is not None:
            unconstrained_text += unconstrained_pick

        constrained_display = sampler.current_text()
        unconstrained_display = unconstrained_text
        print(
            f"  Step {step:2d}: constrained='{repr(next_token)[1:-1]}' | unconstrained='{repr(unconstrained_pick)[1:-1]}'\n"
            f"             constrained_text='{constrained_display}'\n"
            f"             unconstrained_text='{unconstrained_display}'"
        )

    print("\n--- Result ---")
    print(f"Generated: '{sampler.current_text()}'")
    print(f"Complete: {sampler.is_complete()}")

    print("\n--- AST ---")
    try:
        sexpr = sampler.generator.to_sexpr()
        if len(sexpr) > 800:
            print(sexpr[:800] + "\n...")
        else:
            print(sexpr)
    except Exception as e:
        print(f"Cannot serialize: {e}")


if __name__ == "__main__":
    main()
