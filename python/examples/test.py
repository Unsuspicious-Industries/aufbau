import proposition_7 as p7

engine = p7.CompletionEngine(p7.GRAMMARS["xtlc"])
engine.feed("{a:X}{b:Y}((λx:X.x)a)")
completions = engine.debug_completions()
print("Completions:", completions)