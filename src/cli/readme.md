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

## `check`

Type-check a program from stdin (complete or partial).

```sh
echo "<program>" | aufbau check -s <spec>
```

Complete programs print `<program> : <type>`. Partial programs print all
candidate types from surviving parse roots.

| flag    | effect |
|---------|--------|
| `--ast` | print the full typed AST |
| `--all` | show all parse candidates, including duplicates |

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


## Global flags

| flag                | effect |
|---------------------|--------|
| `-v` / `--verbose`  | log level: `-v` warn, `-vv` info, `-vvv` debug, `-vvvv` trace |
| `--trace`           | set log level to trace |
| `--modules <list>`  | comma-separated module filter: `parser`, `grammar`, `bind`, `search`, ... |
| `--with-input`      | include the input string in log messages |
