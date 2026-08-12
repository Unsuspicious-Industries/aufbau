# Admitted obligations

Every `Admitted` in `verification/` is listed here, one per line, as
`<file>:<name>`. `make obligations` regenerates the check; CI fails if a `.v`
grows an `Admitted` that is not in this manifest.

The point is that "proven" claims stay reproducible: an admitted statement is
an assumption, and an assumption nobody wrote down is indistinguishable from a
theorem. Adding a line here is deliberate; it is not a formality.

<!-- BEGIN MANIFEST -->
generation.v:gen_sound
generation.v:gen_complete
<!-- END MANIFEST -->

## Why these two are still open

`gen_sound` and `gen_complete` (Theorems 4.6 / 4.7 of the draft) are stated
against the generic `Generation` functor. Their proofs depend on a per-domain
`step_sound` obligation (Definition 4.8) discharged inside each concrete
`GenDomain` instantiation, which does not exist yet for the real typing domain.

`domain.v` records, as documentation only, what grounding the IR-cut claim
would additionally require. It deliberately adds no `Admitted` of its own.
