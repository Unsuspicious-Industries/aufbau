"""Built-in grammars with typing rules (context-dependant generation)."""

from typing import Any, Dict, List
from pathlib import Path

# Helper function to load grammar specs from examples directory
def _load_spec(name: str) -> str:
    """Load a grammar spec from the examples directory."""
    # Find the examples directory relative to the package root
    package_dir = Path(__file__).parent.parent
    spec_path = package_dir.parent / "examples" / f"{name}.spec"
    
    if not spec_path.exists():
        raise FileNotFoundError(f"Grammar spec not found: {spec_path}")
    
    return spec_path.read_text()

# Unified grammar information: spec content + metadata for prompts
GRAMMARS: Dict[str, Dict[str, Any]] = {
    "stlc": {
        "spec": _load_spec("stlc"),
        "name": "Simply Typed Lambda Calculus",
        "short": "typed lambda calculus",
        "description": "Simply typed lambda calculus with type inference",
        "syntax_hints": [
            "λx:T.e - lambda abstraction (function taking x of type T)",
            "{x:T} - declare variable x of type T in scope",
            "(f e) - function application",
            "Types: base types (Int, Bool) or function types (Int->Bool)",
        ],
        "examples": [
            ("identity", "λx:Int.x"),
            ("const K", "λx:Int.λy:Bool.x"),
            ("apply", "λf:(Int->Bool).λx:Int.(f x)"),
        ],
    },
    "imp": {
        "spec": _load_spec("imp"),
        "name": "IMP",
        "short": "imperative programs",
        "description": "Simple imperative programming language",
        "syntax_hints": [
            "x := e - assignment",
            "skip - no-op",
            "s1; s2 - sequence",
            "if b then s1 else s2 - conditional",
            "while b do s - loop",
        ],
        "examples": [
            ("assignment", "x := 5"),
            ("sequence", "x := 1; y := 2"),
            ("loop", "while x < 10 do x := x + 1"),
        ],
    },
    "clike": {
        "spec": _load_spec("clike"),
        "name": "C-like",
        "short": "C-like code",
        "description": "C-like programming language with types",
        "syntax_hints": [
            "int x; - variable declaration",
            "x = e; - assignment",
            "if (cond) { ... } else { ... } - conditional",
            "while (cond) { ... } - loop",
            "return e; - return statement",
        ],
        "examples": [
            ("declaration", "int x;"),
            ("assignment", "x = 42;"),
            ("function", "int add(int a, int b) { return a + b; }"),
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
