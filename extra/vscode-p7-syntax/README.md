# P7 Grammar & Type Rules Syntax Extension

A Visual Studio Code extension that provides syntax highlighting for P7's EBNF-like grammar and typing rule specifications.

## Features

- **Grammar Production Syntax**: Highlights EBNF-like grammar productions with rule annotations
- **Typing Rules**: Syntax highlighting for inference rules with premises and conclusions
- **Type Expressions**: Full support for complex type expressions including:
  - Function types (`τ₁ → τ₂`)
  - Union and intersection types (`∨`, `∧`)
  - Pointer and array types (`*τ`, `τ[]`)
  - Context types and lookups (`Γ(x)`)
  - Unicode type variables (`τ`, `σ`, `ρ`)
- **Context Management**: Highlighting for typing contexts and transformations
- **Comments**: Support for line comments (`//`)

## Syntax Examples

### Grammar Productions
```
Variable(var) ::= Identifier[x]
Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ₁] '.' Term[e]
Application(app) ::= Term[f] Term[a]
```

### Typing Rules
```
x ∈ Γ
----------- (var)
Γ(x)

Γ[x:τ₁] ⊢ e : τ₂
--------------------------- (lambda)
τ₁ → τ₂

Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
-------------------------------- (app)
τ₂
```

### Type Expressions
```
int → string → bool
(Readable ∧ Writable) ∨ Default
*int[10]
Γ(f) → Γ(x) → Γ(result)
```

## File Extensions

The extension automatically activates for files with:
- `.spec` extensions
- `.p7` extensions

## Installation

1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X)
3. Install this extension
4. Open any `.spec` or `.p7` file to see syntax highlighting

## Contributing

This extension is part of the P7 project for building grammar/semantics/type rules independent AST and typing systems.

## License

MIT
