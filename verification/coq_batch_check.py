#!/usr/bin/env python3
import argparse
import os
import re
import subprocess
import sys
import tempfile


LANG_INFO = {
    "stlc": ("verification.coq.STLC", "STLC.typecheck"),
    "fun": ("verification.coq.Fun", "FunLang.typecheck"),
    "imp": ("verification.coq.Imp", "ImpLang.typecheck_program"),
    "typescript": ("verification.coq.Typescript", "TypescriptLang.typecheck_program"),
}


def coq_escape(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("language", choices=sorted(LANG_INFO.keys()))
    p.add_argument("program_file")
    p.add_argument("--coq-build-dir", required=True)
    return p.parse_args()


def main() -> int:
    args = parse_args()
    coq_import, check_term = LANG_INFO[args.language]

    with open(args.program_file, "r", encoding="utf-8") as fh:
        programs = [line.rstrip("\n") for line in fh if line.rstrip("\n")]

    if not programs:
        return 0

    script = [
        "Require Import Corelib.Strings.PrimStringAxioms.",
        f"Require Import {coq_import}.",
        "Open Scope pstring_scope.",
    ]
    for idx, program in enumerate(programs):
        escaped = coq_escape(program)
        script.append(
            f'Goal True. idtac "BEGIN{idx}". Eval vm_compute in ({check_term} "{escaped}"). '
            f'idtac "END{idx}". exact I. Qed.'
        )

    with tempfile.NamedTemporaryFile("w", suffix=".v", delete=False, encoding="utf-8") as tf:
        tf.write("\n".join(script))
        temp_path = tf.name

    try:
        proc = subprocess.run(
            ["coqtop", "-quiet", "-Q", args.coq_build_dir, "verification.coq"],
            stdin=open(temp_path, "r", encoding="utf-8"),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            check=False,
        )
    finally:
        os.unlink(temp_path)

    out = proc.stdout
    for idx in range(len(programs)):
        begin = out.find(f"BEGIN{idx}")
        end = out.find(f"END{idx}", begin if begin >= 0 else 0)
        chunk = out[begin:end] if begin >= 0 and end >= 0 else ""
        accepted = bool(re.search(r"=\s+Some\b", chunk))
        summary = " ".join(line.strip() for line in chunk.splitlines() if line.strip().startswith("="))
        print(f"{idx}|{'ok' if accepted else 'reject'}|{summary}")

    return 0 if proc.returncode == 0 else proc.returncode


if __name__ == "__main__":
    raise SystemExit(main())
