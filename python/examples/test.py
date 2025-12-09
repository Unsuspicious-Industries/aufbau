import proposition_7 as p7

engine = p7.CompletionEngine(p7.GRAMMARS["xtlc"])

examples = [
    "λa:X.λb:Y.((λx:X.x)",

    "λa:X.λb:Y.((λx:X.x)a",
    "λa:X.λb:Y.((λx:X.x)b",
    
    "λf:X->X. λx:X. f (f",

    "λd:X.λg:(X->X).g",


    # ill-typed
    "λx:X. x x",              
    "λf:(X->Y). f f",         
]

examples = [
    "λx: X. λy: (X->X)->Y. y"
]

for expr in examples:
    engine.reset()
    print("---")
    print(expr)
    try:
        # Thee "feed" does the actual type checking
        # If you feed an ill-typed expression, it raises TypeError
        engine.feed(expr)
        # The produced set of completions is not perfectly well typed
        # It doesnt scan ahead to filter only well typed contituations
        # in only check is the trees are well typed so far,
        # And can be continued in a well typed manner.
        # Thi means it can produce BAD completions
        completions = engine.debug_completions()
        print("Examples:", completions['examples'])
        print("Set:", completions['patterns'])
    except TypeError as te:
        print(f"{te}")
