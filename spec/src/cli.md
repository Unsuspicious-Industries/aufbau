#[D] CLI Reference

The `aufbau` binary is the command-line entry point for the toolkit.  It
groups functionality into three top-level subcommands and a shared set of
global flags that control debug output.

## Global Flags

These flags apply to every subcommand and are declared with `global = true` in
`src/cli/mod.rs`.

| Flag | Short | Type | Default | Description |
|------|-------|------|---------|-------------|
| `--verbose` | `-v` | count | 0 | Increase verbosity; repeat up to three times |
| `--trace` | | bool | false | Force trace-level debug output (overrides `--verbose`) |
| `--modules` | | `LIST` | none | Comma-separated module filter (e.g. `parser,grammar,bind,check`) |
| `--with-input` | | bool | false | Annotate span messages with the raw input text |

Verbosity levels map as follows:

| `-v` count | `--trace` | Effective level |
|-----------|-----------|-----------------|
| 0 | false | `Error` |
| 1 | false | `Warn` |
| 2 | false | `Info` |
| 3 | false | `Debug` |
| any | true | `Trace` |

## Subcommands

| Subcommand | Description |
|------------|-------------|
| `logic` | Parser, completion, and visualization tools |
| `validate` | Run built-in validation test suites |
| `examine` | Inspect a single input or named test case |

---

## `aufbau logic`

The `logic` group (`src/cli/logic/mod.rs`) contains two subcommands.

### `aufbau logic viz`

Launches the interactive visualization server.

```
aufbau logic viz [--port <PORT>] [--spec <FILE>]
```

| Flag | Short | Default | Description |
|------|-------|---------|-------------|
| `--port` | `-p` | `5173` | Port to bind on `127.0.0.1` |
| `--spec` | `-s` | none | Grammar spec file to pre-load |

The server serves a browser-based UI at `http://127.0.0.1:<PORT>/`.  See the
[Visualization Server](viz.md) chapter for the full HTTP API reference.

### `aufbau logic complete`

Prints the set of valid next tokens for a partial input.

```
aufbau logic complete --spec <FILE> (--input <TEXT> | --file <FILE>) [options]
```

| Flag | Short | Description |
|------|-------|-------------|
| `--spec` | `-s` | Grammar spec file (required) |
| `--input` | `-i` | Partial input as an inline string |
| `--file` | `-f` | Path to a file containing the partial input |
| `--start` | | Override the grammar start symbol |
| `--max` | `-k` | Maximum number of completions to print |
| `--show-details` | | Print metadata alongside each completion |

Exactly one of `--input` or `--file` must be provided.  The command exits 0
on success, 1 on a parse error, and 2 on a configuration error.

---

## `aufbau validate`

Runs the built-in validation test suites (`src/cli/validate/`).

```
aufbau validate [--module <MODULE>] [--filter <STR>] [--jobs <N>] [--profile <FILE>]
```

| Flag | Short | Description |
|------|-------|-------------|
| `--module` | `-m` | Run only one module: `completable`, `parseable`, or `complexity` |
| `--filter` | `-f` | Filter suites by name substring (e.g. `stlc`, `fun::lambda`) |
| `--jobs` | `-j` | Worker thread count (default: Rayon pool size) |
| `--profile` | | Write a Chrome trace JSON for profiling |

When `--module` is omitted all three modules run sequentially.

### Validation Modules

| Module | Description |
|--------|-------------|
| `completable` | BFS completion search: checks that every test input can be completed to a valid, well-typed program within the configured depth |
| `parseable` | Fast prefix-parsing checks: verifies that all prefixes of test inputs parse without error |
| `complexity` | Measures parse time and tree-count growth across inputs of increasing length |

---

## `aufbau examine`

Interactive debugging tool for a single input or a named built-in test case.
Prints the partial AST, typed AST, completion sets, and full test metadata.

```
aufbau examine (--case <DESC> | --input <TEXT> --spec <FILE>) [options]
```

### Mode 1: Named test case

```
aufbau examine --case <DESC> [--filter <STR>] [--depth <N>] [--expected <OUTCOME>]
               [--dump-ast] [--dump-completions]
```

Searches all built-in validation suites for a case whose description contains
`<DESC>`.  The first match is selected and run through the full test harness.
If `--filter` is given, only suites whose name contains that substring are
searched.

### Mode 2: Ad-hoc input

```
aufbau examine --input <TEXT> --spec <FILE> [--sound] [--depth <N>]
               [--dump-ast] [--dump-completions]
```

Parses `<TEXT>` against the grammar in `<FILE>` and either attempts a
completion search (`--sound` off) or a prefix-soundness check (`--sound`).

### Examine Flags

| Flag | Short | Default | Description |
|------|-------|---------|-------------|
| `--spec` | `-s` | | Grammar spec file (required for `--input` mode) |
| `--input` | `-i` | | Raw partial input string |
| `--case` | `-c` | | Test case description substring |
| `--filter` | `-f` | | Suite name filter for `--case` mode |
| `--expected` | | | Expected outcome (`ok`, `fail`, `type_error`) |
| `--sound` | | false | Run prefix-soundness check instead of single completion |
| `--depth` | | `10` | Maximum BFS/DFS search depth |
| `--dump-ast` | | false | Print the full PartialAST and TypedAST debug structures |
| `--dump-completions` | | false | Print raw and typed completion sets |

### Output

On success the examine command prints:

- The PartialAST root count (and full debug dump with `--dump-ast`)
- The TypedAST root count (or a typing error)
- Raw and typed completion sets (with `--dump-completions`)
- Test result: `PASS` or `FAIL` with duration
- Extended metadata: `states_explored`, `prefixes_checked`, per-prefix timing
- On pass: the full completed output string and a serialized `.ast` file written
  to `validation/trees/`

Exit codes: 0 = pass, 1 = fail, 2 = configuration error.
