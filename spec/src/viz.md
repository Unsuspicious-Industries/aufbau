#[D] Visualization Server

The visualization server (`src/viz/`) is an HTTP server that exposes the
parser, type-checker, and completion engine through a small JSON API.  It
serves a single-page web application from embedded static assets and is
launched via `aufbau logic viz`.

<!-- DIAGRAM: architecture -- browser <-> HTTP server <-> Grammar / Parser / TypeChecker -->

## Launching

```
aufbau logic viz [--port <PORT>] [--spec <FILE>]
```

| Flag | Default | Description |
|------|---------|-------------|
| `-p` / `--port` | `5173` | TCP port to bind on `127.0.0.1` |
| `-s` / `--spec` | none | Optional grammar spec to pre-load in the UI |

The server prints its address to stderr and blocks until killed.

## Routes

All routes are defined in `src/viz/server.rs`.

| Method | Path | Handler | Description |
|--------|------|---------|-------------|
| `GET` | `/` | embedded HTML | Serves the single-page application |
| `GET` | `/specs` | `handle_list_specs` | Lists `.spec` files in `examples/` |
| `POST` | `/graph` | `handle_parser_viz_request` | Legacy graph endpoint |
| `POST` | `/analyze` | `handle_analyze_request` | Full analysis endpoint |
| `GET` | `/static/{file}` | embedded assets | `app.js`, `styles.css` |
| `GET` | `/examples/{file}` | filesystem | Reads `examples/*.auf` (allowlisted) |

Static files and the index HTML are compiled into the binary with
`include_str!`, so the server has no runtime file dependencies beyond the
`examples/` directory for spec files.

## Request Types

### `GraphRequest` (legacy `/graph`)

Sent as JSON to `POST /graph`.  Returns a `ParseResponse`.

| Field | Type | Description |
|-------|------|-------------|
| `spec` | `String` | Grammar specification text |
| `input` | `String` | Partial input to parse |

### `AnalyzeRequest` (`/analyze`)

Sent as JSON to `POST /analyze`.  Returns an `AnalyzeResponse`.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `spec` | `String` | required | Grammar specification text |
| `input` | `String` | required | Partial input to analyze |
| `debug_level` | `String?` | `null` | Server-side debug verbosity (`none`, `error`, `warn`, `info`, `debug`, `trace`) |
| `debug_modules` | `[String]` | `[]` | Module filter list (e.g. `["parser", "grammar"]`) |
| `context` | `[ContextBinding]` | `[]` | Initial typing context entries |

`ContextBinding` has two string fields: `name` (variable name) and `ty` (type
string).  The server parses these into `Type::Raw(ty)` atoms and injects them
into the typing context before running the type-checker.

## Response Types

### `AnalyzeResponse`

The primary response returned by `/analyze`.

| Field | Type | Description |
|-------|------|-------------|
| `version` | `"v1"` | Protocol version tag |
| `ok` | `bool` | `false` on parse or spec errors |
| `warnings` | `[String]` | Non-fatal issues (e.g. typed AST unavailable) |
| `error` | `String?` | Error message when `ok = false` |
| `is_complete` | `bool` | Whether the partial AST is syntactically complete |
| `root_count` | `usize` | Number of parse trees in the forest |
| `tokens` | `[TokenInfo]` | Token list for UI highlighting |
| `ast_graph` | `GraphData` | Node/edge representation of the parse forest |
| `typed_ast` | `TypedAstResponse?` | Typed AST summary, or `null` on type error |
| `completions` | `[String]` | Well-typed completion strings |
| `all_completions` | `[String]` | All syntactic completions (superset of `completions`) |
| `timings_ms` | `TimingsMs` | Per-phase latency in milliseconds |

### `TimingsMs`

All fields are `u128` milliseconds.

| Field | Phase |
|-------|-------|
| `grammar_load` | Grammar parsing |
| `tokenize` | Tokenization of input |
| `parse_partial` | Partial AST construction |
| `build_graph` | Graph serialization |
| `completions` | Completion search and filtering |
| `typed_ast` | Type inference |
| `total` | Wall time for the entire request |

### `TokenInfo`

Represents one token emitted by the tokenizer.

| Field | Type | Description |
|-------|------|-------------|
| `text` | `String` | Raw token text |
| `start` | `usize` | Start byte offset in input |
| `end` | `usize` | End byte offset in input |
| `index` | `usize` | Sequential token index |
| `is_partial_special` | `bool` | True for the synthetic partial-token placeholder |

### `TypedAstResponse`

Summary of the typed AST overlay.

| Field | Type | Description |
|-------|------|-------------|
| `trees` | `[TypedTreeInfo]` | Per-root-tree metadata |
| `roots` | `[TypedNodeResponse]` | Typed node trees (one per parse root) |

### `TypedTreeInfo`

| Field | Type | Description |
|-------|------|-------------|
| `index` | `usize` | Root index (matches `roots[index]`) |
| `complete` | `bool` | Syntactically complete |
| `type_status` | `String` | `"valid"`, `"partial"`, `"malformed"`, or `"too_deep"` |
| `ty` | `String` | Display string of the inferred type |

### `TypedNodeResponse`

A tagged enum serialized with `"kind"` as the discriminant.

| Variant | Fields | Description |
|---------|--------|-------------|
| `"Term"` | `val: String`, `ty: String` | Terminal node with inferred type |
| `"Expr"` | `name`, `ty`, `complete`, `children` | Non-terminal with typed children |

## Graph Data (`GraphData`)

Returned inside `AnalyzeResponse.ast_graph` and used by the front-end to
render the parse forest as a directed graph.

| Field | Type | Description |
|-------|------|-------------|
| `nodes` | `[GraphNode]` | All graph nodes |
| `edges` | `[GraphEdge]` | Directed edges between nodes |
| `trees` | `[TreeInfo]` | Per-root-tree summary |
| `reconstructed_inputs` | `[ReconstructedInput]` | Input text reconstructed from each tree |

`GraphNode` carries a `status` string (`"complete"`, `"partial"`, `"terminal"`,
`"error"`) and a `meta` object (`NodeMeta`) with the production rule details,
inferred type, typing rule, typing constraints, and context bindings visible at
that node.

`GraphEdge` carries a `style` field (`"solid"` for well-typed trees, `"dashed"`
for ill-typed ones) so the UI can visually distinguish valid and invalid parse
paths.

## Completion Pipeline

The `/analyze` handler computes two completion lists via
`compute_completions_for_partial` (`src/viz/viz.rs`):

1. All syntactic completions: tokens generated from `PartialAST::completions`
   and resolved via `find_working_completion` (symbol-priority heuristic then
   regex examples).
2. Well-typed completions: filtered subset where extending the input with the
   completion token yields at least one `Valid` or `Partial` tree after
   re-parsing.

The UI renders the filtered list as primary completions and the superset list
as a reference panel.
