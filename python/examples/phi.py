#!/usr/bin/env python3
"""
Phi-3.5-mini demo: ReasoningEnvironment with CoT + Grammar-constrained output.

Demonstrates the new ReasoningEnvironment that allows the model to:
1. Think freely in <think>...</think> blocks (unconstrained CoT)
2. Produce typed output in <stlc>...</stlc> blocks (grammar-constrained)

Uses Microsoft's Phi-3.5-mini-instruct.
https://huggingface.co/microsoft/Phi-3.5-mini-instruct
"""

import proposition_7 as p7


# Tasks for the environment to solve
TASKS = [
    {
        "name": "identity",
        "task": "Create the identity function for Int",
        "initial": "λx:",
        "description": "Should produce λx:Int.x",
    },
    {
        "name": "const_K",
        "task": "Create the K combinator (constant function) that takes an Int, then a Bool, and returns the Int",
        "initial": "λx:Int.",
        "description": "Should produce λx:Int.λy:Bool.x",
    },
    {
        "name": "apply",
        "task": "Create a function that applies a function f:(Int->Bool) to an argument x:Int",
        "initial": "λf:(Int->Bool).",
        "description": "Should produce λf:(Int->Bool).λx:Int.(f x)",
    },
    {
        "name": "compose",
        "task": "Create the B combinator (function composition) for f:(Int->Bool) and g:(Bool->Int)",
        "initial": "λf:(Int->Bool).",
        "description": "Should produce something like λf:(Int->Bool).λg:(Bool->Int).λx:Bool.(f (g x))",
    },
    {
        "name": "twice",
        "task": "Create a function that applies f:(Int->Int) twice to x:Int",
        "initial": "λf:(Int->Int).",
        "description": "Should produce λf:(Int->Int).λx:Int.(f (f x))",
    },
]


def print_header(text: str, char: str = "=", width: int = 80):
    print(char * width)
    print(text)
    print(char * width)


def run_environment_demo(env: p7.ReasoningEnvironment):
    """Run the ReasoningEnvironment demo with all tasks."""
    
    results = []
    
    for task in TASKS:
        print_header(f"Task: {task['name']}", "=")
        print(f"  Description: {task['description']}")
        print(f"  Task:        {task['task']}")
        print(f"  Initial:     {task['initial']}")
        print()
        
        # Callbacks for streaming output
        def on_mode_switch(mode: p7.Mode, tag: str):
            print(f"\n--- Mode: {mode.value} ({tag}) ---")
        
        def on_think_token(tok: str, step: int):
            print(tok, end="", flush=True)
        
        def on_grammar_token(tok: str, step: int):
            print(f"\033[92m{tok}\033[0m", end="", flush=True)  # Green for grammar
        
        try:
            result = env.generate(
                prompt=task["task"],
                initial=task["initial"],
                max_blocks=4,
                start_thinking=True,
                on_mode_switch=on_mode_switch,
                on_think_token=on_think_token,
                on_grammar_token=on_grammar_token,
            )
            
            print("\n")
            print_header("Result", "-", 40)
            print(f"  Complete:      {result.is_complete}")
            print(f"  Total tokens:  {result.total_tokens}")
            print(f"  Stop reason:   {result.stopped_reason}")
            print(f"  Think blocks:  {len(result.think_blocks)}")
            print(f"  Grammar blocks: {len(result.grammar_blocks)}")
            
            if result.final_output:
                print(f"\n  Final output:  {result.final_output.content}")
            
            results.append({
                "name": task["name"],
                "complete": result.is_complete,
                "output": result.final_output.content if result.final_output else None,
                "tokens": result.total_tokens,
            })
            
        except Exception as e:
            print(f"\n  ERROR: {e}")
            results.append({
                "name": task["name"],
                "complete": False,
                "output": None,
                "tokens": 0,
                "error": str(e),
            })
        
        print("\n")
    
    return results


def run_simple_demo(model: p7.ConstrainedModel, grammar_name: str):
    """Run the SimpleEnvironment demo."""
    
    print_header("SimpleEnvironment Demo", "#")
    print("Single-shot CoT + Grammar generation\n")
    
    env = p7.SimpleEnvironment(
        model=model,
        grammar_name=grammar_name,
        cot_tokens=100,
        grammar_tokens=50,
    )
    
    task = "Create a function that takes an Int and returns an Int"
    initial = "λx:Int."
    
    print(f"Task: {task}")
    print(f"Initial: {initial}")
    print()
    
    print("--- Chain of Thought ---")
    cot, output, is_complete = env.generate(
        task=task,
        initial=initial,
        on_cot=lambda x: print(x),
        on_output=lambda x: print(f"\n--- Output ---\n\033[92m{x}\033[0m"),
    )
    
    print(f"\nComplete: {is_complete}")
    print(f"Final: {output}")


def main():
    print_header("P7 ReasoningEnvironment Demo with Phi-3.5-mini", "#", 80)
    
    model_name = "microsoft/Phi-3.5-mini-instruct"
    grammar_name = "stlc"
    
    print(f"\nLoading {model_name}...")
    print(f"Grammar: {grammar_name}")
    print()
    
    # Show the auto-generated system prompt
    system_prompt = p7.build_system_prompt(grammar_name)
    print_header("Auto-generated System Prompt", "-", 60)
    print(system_prompt)
    print()
    
    # Load model
    model = p7.ConstrainedModel.from_pretrained(
        model_name,
        grammar=p7.get_grammar(grammar_name),
        device_map="auto",
        torch_dtype="auto",
        trust_remote_code=True,
    )
    print(f"Vocabulary size: {len(model.vocab)}")
    print()
    
    # Create reasoning environment
    env = p7.ReasoningEnvironment(
        model=model,
        grammar_name=grammar_name,
        think_budget=150,
        grammar_budget=50,
    )
    
    # Run main demo
    print_header("ReasoningEnvironment Demo", "#")
    print(f"CoT reasoning with <think> blocks, typed output with <{grammar_name}> blocks\n")
    
    results = run_environment_demo(env)
    
    # Summary
    print_header("SUMMARY", "=")
    print(f"\n{'Task':<15} {'Complete':<10} {'Output':<40}")
    print("-" * 65)
    for r in results:
        output = r['output'][:35] + "..." if r['output'] and len(r['output']) > 35 else (r['output'] or "N/A")
        print(f"{r['name']:<15} {str(r['complete']):<10} {output:<40}")
    
    complete = sum(1 for r in results if r['complete'])
    print("-" * 65)
    print(f"Complete: {complete}/{len(results)}")
    
    # Also run simple demo
    print("\n")
    run_simple_demo(model, grammar_name)
    
    print("\n")
    print_header("KEY INSIGHT", "=")
    print("""
  The ReasoningEnvironment enables structured generation where:
  
  1. <think>...</think> blocks allow free-form reasoning (unconstrained)
  2. <{grammar}>...</{grammar}> blocks produce well-typed output (constrained)
  
  This combines the flexibility of Chain-of-Thought reasoning with the
  guarantees of grammar-constrained generation.
  
  Tags are grammar-specific: <stlc>, <fun>, <imp>, etc. - not a generic <formal>.
""")


if __name__ == "__main__":
    main()
