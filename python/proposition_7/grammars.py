"""Built-in grammars with typing rules (context-dependent generation)."""

from pathlib import Path
from typing import Any, Dict, List


def _load_spec(name: str) -> str:
    """Load a grammar spec from the repository examples directory."""
    repo_root = Path(__file__).resolve().parents[2]
    spec_path = repo_root / "examples" / f"{name}.spec"

    if not spec_path.exists():
        raise FileNotFoundError(f"Grammar spec not found: {spec_path}")

    return spec_path.read_text(encoding="utf-8")


# Unified grammar information: spec content + metadata for prompt construction.
GRAMMARS: Dict[str, Dict[str, Any]] = {
    "stlc": {
        "spec": _load_spec("stlc"),
        "name": "Simply Typed Lambda Calculus",
        "short": "typed lambda calculus terms",
        "description": "Simply typed lambda calculus with explicit type annotations",
        "syntax_hints": [
            "λx:T.e - lambda abstraction",
            "(f x) - function application",
            "Types use -> and are right-associative: Int->Bool->Int",
            "Parenthesize function arguments and nested types when needed",
        ],
        "examples": [
            ("identity", "λx:Int.x"),
            ("const", "λx:Int.λy:Bool.x"),
            ("apply", "λf:(Int->Bool).λx:Int.(f x)"),
        ],
    },
    "imp": {
        "spec": _load_spec("imp"),
        "name": "IMP",
        "short": "typed imperative programs",
        "description": "Typed imperative language with assignments, conditionals, and loops",
        "syntax_hints": [
            "Assignment: x: Type = value;",
            "Arithmetic values: x + y, x - 1, a * b",
            "Conditionals: if cond { ... } else { ... }",
            "Loops: while cond { ... }",
            "Type unions are allowed: Int|Bool",
        ],
        "examples": [
            ("assignment", "x: Int = 5;"),
            ("sequence", "x: Int = 1; y: Int = x + 2;"),
            ("if_else", "x: Int = 1; if x < 5 { y: Int = x + 1; } else { y: Int = 0; }"),
            ("while", "counter: Int = 0; while counter < 3 { counter + 1; }"),
        ],
    },
    "fun": {
        "spec": _load_spec("fun"),
        "name": "Fun",
        "short": "typed functional expressions",
        "description": "ML-style functional language with let bindings and typed lambdas",
        "syntax_hints": [
            "Lambda: (x: Type) => expr",
            "Let binding: let x: Type = value; body",
            "Function application: f(arg)",
            "Int ops: + - * /, Float ops: +. -. *. /.",
            "Literals include Int, Float, and Bool",
        ],
        "examples": [
            ("identity", "(x: Int) => x"),
            ("let_int", "let x: Int = 1; x + 2"),
            ("apply_lambda", "((x: Int) => x + 1)(41)"),
            ("float_math", "let f: Float = 1.5; f +. 2.0"),
        ],
    },
}


def list_grammars() -> List[str]:
    """List all available grammar names."""
    return list(GRAMMARS.keys())


def get_grammar(name: str) -> str:
    """Get the spec content for a grammar."""
    if name not in GRAMMARS:
        available = ", ".join(GRAMMARS.keys())
        raise ValueError(f"Unknown grammar '{name}'. Available: {available}")
    return GRAMMARS[name]["spec"]


def get_grammar_info(grammar_name: str) -> Dict[str, Any]:
    """Get info about a grammar, with fallback for unknown grammars."""
    if grammar_name in GRAMMARS:
        return GRAMMARS[grammar_name]
    # Fallback for unknown grammars
    return {
        "spec": "",
        "name": grammar_name,
        "short": f"{grammar_name} expressions",
        "description": f"Grammar: {grammar_name}",
        "syntax_hints": [],
        "examples": [],
    }
