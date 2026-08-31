"""The base grammar library: one copy of each `.auf`, shared by every consumer.

This directory *is* the source of truth. The Rust crate reads the files beside
this module directly -- `include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
"/examples/ml.auf"))` and friends, some thirty sites -- so rather than copy them
anywhere, this module publishes the same directory to Python:

    from aufbau import examples

    examples.names()      # ['c', 'fun', 'imp', 'ml', 'stlc', 'sums', 'toy']
    examples.spec("ml")   # the source text
    examples.path("ml")   # the file, for tools that want a path

Co-location is deliberate. The obvious alternative -- a loader under
`python/aufbau/` that goes looking for the grammars -- breaks in this repo: from
the checkout root, `import aufbau` binds the *crate directory* as a namespace
package rather than the installed wheel, so a loader elsewhere in the tree is
silently shadowed and resolves to nothing. Here there is nothing to resolve:
the specs are `Path(__file__).parent`. `python/aufbau/examples` is a symlink to
this directory, which is what puts it in the wheel, the same one-source-of-truth
trick `python/aufbau/__init__.pyi -> ../../aufbau.pyi` already uses.

The duplication this replaces was not hypothetical. `p7/src/grammars/c.auf` sat
sixty lines behind this copy: the bundled snapshot predated both the return-type
constraint and the `ForInit` split, so it still carried the genuine dead end at
`for (` that the split exists to remove. A copy asserts that two files are
equal, and nothing was checking it.

Consumers with grammars of their own -- p7's `typescript.auf`, gamma's D3
placeholder -- keep hosting those locally. Only the base library is shared.
"""

from __future__ import annotations

import os
from pathlib import Path

__all__ = ["names", "path", "spec", "specs", "root"]

#: Point this at another directory to work against a different grammar library
#: (an installed wheel's copy versus a checkout's, say) without reinstalling.
ENV_VAR = "AUFBAU_EXAMPLES"


def root() -> Path:
    """The directory the specs are read from."""
    override = os.environ.get(ENV_VAR)
    return Path(override) if override else Path(__file__).resolve().parent


def names() -> list[str]:
    """Every base grammar, sorted. Adding a `.auf` here publishes it."""
    return sorted(p.stem for p in root().glob("*.auf"))


def path(name: str) -> Path:
    """The file for `name`, which carries no `.auf` suffix."""
    p = root() / f"{name}.auf"
    if not p.is_file():
        raise FileNotFoundError(
            f"no base grammar {name!r} in {root()}; have {', '.join(names())}"
        )
    return p


def spec(name: str) -> str:
    """The source text of `name`."""
    return path(name).read_text(encoding="utf-8")


def specs() -> dict[str, str]:
    """Every base grammar, as {name: source}."""
    return {n: spec(n) for n in names()}
