#!/usr/bin/env python3
"""Assert the built `aufbau` module exposes exactly what `aufbau.pyi` promises.

This is the regression the missing-methods aarch64 wheel would have caught: the
published 0.2.1 aarch64 `.so` silently lacked `Synthesizer.from_grammar`,
`mask`, `status`, `root_type` and `in_scope`, while x86_64 was fine. Nothing
compared the shipped artifact against its own type stub, so it went out.

Run against an *installed* wheel, not the source tree:

    pip install dist/aufbau_rs-*.whl
    python scripts/check_wheel_api.py

The stub is the contract, so every class in it is checked. Names are read out of
the stub rather than listed here: a class added to `aufbau.pyi` is covered
without touching this script.
"""

from __future__ import annotations

import ast
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
STUB = os.path.join(os.path.dirname(HERE), "aufbau.pyi")


def stub_module_names(path: str) -> list[str]:
    """Module-level names the stub declares, e.g. `ENGINE_API: str`.

    `ENGINE_API` is the whole point of the release contract — a consumer checks
    it instead of probing for methods — so a wheel that ships without it is
    broken in exactly the way this script exists to catch.
    """
    tree = ast.parse(open(path).read(), filename=path)
    return [
        node.target.id
        for node in tree.body
        if isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name)
    ]


def stub_api(path: str) -> dict[str, dict[str, str]]:
    """{class name: {member name: "method" | "attr"}} as declared in the stub.

    The kind matters: `def rhs(self)` and `rhs: list[Symbol]` are different
    contracts, and a `#[getter]` on the Rust side satisfies only the second.
    Checking presence alone would let `p.rhs()` in the stub pass against a
    property that must be spelled `p.rhs`.
    """
    tree = ast.parse(open(path).read(), filename=path)
    api: dict[str, dict[str, str]] = {}
    for node in tree.body:
        if not isinstance(node, ast.ClassDef):
            continue
        members: dict[str, str] = {}
        for item in node.body:
            if isinstance(item, (ast.FunctionDef, ast.AsyncFunctionDef)):
                members[item.name] = "method"
            elif isinstance(item, ast.AnnAssign) and isinstance(item.target, ast.Name):
                members[item.target.id] = "attr"
        api[node.name] = members
    return api


def main() -> int:
    try:
        import aufbau
    except ImportError as e:
        print(f"FAIL: cannot import aufbau: {e}")
        return 1

    origin = getattr(aufbau, "__file__", "<unknown>")
    print(f"checking {origin}")
    print(f"against  {STUB}")

    api = stub_api(STUB)
    if not api:
        print(f"FAIL: no classes parsed from {STUB}")
        return 1

    failures: list[str] = []
    for name in stub_module_names(STUB):
        if hasattr(aufbau, name):
            print(f"  ok {name} = {getattr(aufbau, name)!r}")
        else:
            failures.append(f"{name}: missing module attribute")

    for cls_name, expected in sorted(api.items()):
        cls = getattr(aufbau, cls_name, None)
        if cls is None:
            failures.append(f"{cls_name}: missing from module")
            continue
        problems: list[str] = []
        for member, kind in sorted(expected.items()):
            if not hasattr(cls, member):
                problems.append(f"missing {member}")
                continue
            # A method is callable on the class; a `#[getter]` is a descriptor.
            got = getattr(cls, member)
            if kind == "method" and not callable(got):
                problems.append(f"{member} is an attribute, stub says method")
            elif kind == "attr" and callable(got):
                problems.append(f"{member} is a method, stub says attribute")
        if problems:
            failures.append(f"{cls_name}: " + "; ".join(problems))
        else:
            print(f"  ok {cls_name} ({len(expected)} members)")

    if failures:
        print("\nFAIL: built module does not match aufbau.pyi")
        for f in failures:
            print(f"  - {f}")
        return 1

    print(f"\nOK: {len(api)} classes match aufbau.pyi")
    return 0


if __name__ == "__main__":
    sys.exit(main())
