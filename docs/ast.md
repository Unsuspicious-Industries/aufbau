# AST Serialization

S-expression format for complete ASTs. Testing/debugging only.

## Format

```lisp
;!ast 1
;!rules: rule1, rule2

(N Root ...)
```

## Nodes

**Terminal**: `(T "value")` or `(T "value" (b binding))`

**Nonterminal**: `(N Name)` or `(N Name (rule r) (b b) children...)`

## Example

```lisp
;!ast 1
;!rules: abs, var

(N Expression
  (N Abstraction (rule abs)
    (T "λ")
    (N Identifier (b x) (T "x"))
    (T ":")
    (N Type (T "Int"))
    (T ".")
    (N Expression
      (N Variable (rule var)
        (N Identifier (T "x"))))))
```

## Limitations

- Complete ASTs only
- Bound rule internals not preserved
- Re-binding requires grammar

## See Also

- [grammar.md](grammar.md)
- [partial.md](partial.md)
