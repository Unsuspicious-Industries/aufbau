# aufbau CLI

All commands take `-s <spec>`. Input is read from stdin unless noted.
Exit codes: `0` success, `1` logical failure, `2` usage/IO error.

---

## `check`

Run parseable validation and completable validation.

```sh
aufbau check -j 8 --completable-timeout-secs 30
```

Use `--skip-parseable` or `--skip-completable` to narrow the run.

---

## `typecheck`

Type-check a program from stdin (complete or partial).

```sh
echo "<program>" | aufbau typecheck -s <spec>
```

Complete programs print `<program> : <type>`. Partial programs print all
candidate types from surviving parse roots.

| flag    | effect |
|---------|--------|
| `--ast` | print the full typed AST |
| `--all` | show all parse candidates, including duplicates |

---

## `complete`

Find a well-typed completion for a partial program from stdin.

```sh
echo "<prefix>" | aufbau complete -s <spec>
```

Extends the prefix one token at a time using a priority-guided search.
Writes the completed program to stdout.

| flag             | default | effect |
|------------------|---------|--------|
| `--depth <N>`    | 10      | max token-extension steps |
| `--states <N>`   | 96      | search-state budget |
| `--children <N>` | 12      | beam width per state |
| `--examples <N>` | 1       | candidates tried per regex token |
| `-i` / `--info`  | off     | print search metadata to stderr |
| `--dump-visited` | off     | print visited states to stderr on failure |

---

## `logic complete`

List the valid next tokens for a partial input.

```sh
aufbau logic complete -s <spec> --input "<prefix>"
aufbau logic complete -s <spec> --file <path>
```

One-step lookahead only; not a full synthesis.

| flag           | effect |
|----------------|--------|
| `--start <NT>` | override the start non-terminal |
| `-k` / `--max` | limit output to the first N tokens |

---

## `logic viz`

Start the visualisation server.

```sh
aufbau logic viz [-p <port>] [-s <spec>]
```

Serves a web UI at `http://127.0.0.1:5173`. Default port: `5173`.

---

## `validate`

Run the built-in test suites.

```sh
aufbau validate [-m <module>] [-f <filter>] [-j <N>] [--profile <file>] [--completable-timeout-secs <N>]
```

Modules (`-m`): `completable`, `parseable`. Omit to run both.
Reports are written to `src/validation/reports/`.

| flag               | effect |
|--------------------|--------|
| `-f <substr>`      | filter suites by name |
| `-j <N>`           | parallel worker threads |
| `--completable-timeout-secs <N>` | override per-case completable timeout |
| `--profile <file>` | write perf and failure JSON profiles |

---

## `examine`

Run the completion search on a single input or named test case.

```sh
aufbau examine -s <spec> --input "<prefix>"
aufbau examine --case "<desc>" [-f <suite>]
```

`--case` matches by description substring against the built-in suites.
Completed trees are written to `validation/trees/`.

| flag                                | effect |
|-------------------------------------|--------|
| `--depth <N>`                       | max search depth (default 10) |
| `--sound`                           | check that every prefix is completable |
| `--expected <ok\|fail\|type-error>` | assert expected outcome |
| `--dump-ast`                        | print AST structures |
| `--dump-completions`                | print completion sets |

---

## Global flags

| flag                | effect |
|---------------------|--------|
| `-v` / `--verbose`  | log level: `-v` warn, `-vv` info, `-vvv` debug, `-vvvv` trace |
| `--trace`           | set log level to trace |
| `--modules <list>`  | comma-separated module filter: `parser`, `grammar`, `bind`, `search`, ... |
| `--with-input`      | include the input string in log messages |
