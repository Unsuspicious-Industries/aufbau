#!/usr/bin/env python3
"""GPT-2 demo"""

import sys
sys.path.insert(0, '/home/pkd/code/p7/python')

import torch
from transformers import GPT2LMHeadModel, GPT2Tokenizer
import proposition_7 as p7


def main():
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
    
    input_ids = None
    
    def get_logits() -> list[float]:
        nonlocal input_ids
        with torch.no_grad():
            outputs = model(input_ids)
            logits = outputs.logits[0, -1, :].tolist()
        return logits
    
    print("Creating sampler with stlc grammar...")
    sampler = p7.TypedSampler(
        grammar=p7.GRAMMARS["stlc"]["spec"],
        vocab=vocab,
        logit_fn=get_logits
    )
    
    prompt = "Complete this simply typed lambda calculus expression the shortest way:\n"
    initial_code = "λf:(A->B).λx:A."
    
    print(f"\nPrompt: '{prompt}'")
    print(f"Initial: '{initial_code}'")
    
    prompt_ids = tokenizer.encode(prompt + initial_code, return_tensors="pt")
    input_ids = prompt_ids
    
    sampler.feed(initial_code)
    print(f"Sampler state: '{sampler.current_text()}'")
    unconstrained_text = sampler.current_text()
    
    print("\n--- Generating (constrainted to well-typed) ---")
    max_tokens = 15
    pre_top_k = 100
    greedy_k = 1
    
    for step in range(max_tokens):
        next_token = sampler.infer_greedy(k=greedy_k, pre_top_k=pre_top_k)
        unconstrained_pick = sampler.infer_unconstrained(k=greedy_k)

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
