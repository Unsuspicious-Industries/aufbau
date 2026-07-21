# Aufbau verification — Rocq build pipeline.
#
# Designed for Rocq 9.1.1 + Stdlib 9.0.0 (see ../flake.nix).
# Enter the dev shell with `nix develop` from the repo root first.
#
# Every artifact (.vo, .vok, .vos, .glob, .aux) lands in .build/ — sources stay
# clean.  vsrocq picks up the build cache via the `-Q .build AufbauVerif`
# entry in _CoqProject, so as long as `make build` has been run at least
# once the editor resolves `From AufbauVerif Require …` correctly.

ROCQ        ?= rocq
ROCQ_FLAGS  ?= -w -deprecated-from-Coq,-deprecated-since-9.0,-deprecated
COQPROJECT  := _CoqProject
BUILD_DIR   := .build

# Both loadpaths are needed: `.` so the compiler assigns the correct
# logical name to each source, `.build` so dependent compilations and
# vsrocq find the previously-built .vo files.
LOAD_FLAGS  := -Q . AufbauVerif -Q $(BUILD_DIR) AufbauVerif

VFILES := $(shell awk '/^[^-#]/ && /\.v$$/ {print $$1}' $(COQPROJECT))

.PHONY: all build check clean watch print-files

all: build

# Compile sources in the order listed in _CoqProject (already topological
# for this project).  `rocq compile -o` redirects the .vo output; the
# adjacent .glob/.vok/.vos/.aux artifacts follow it to $(BUILD_DIR).
build: | $(BUILD_DIR)
	@for f in $(VFILES); do \
	  out="$(BUILD_DIR)/$$(basename $$f .v).vo"; \
	  echo "  → rocq compile $$f -> $$out"; \
	  $(ROCQ) compile $(ROCQ_FLAGS) $(LOAD_FLAGS) -o "$$out" $$f || exit 1; \
	done

$(BUILD_DIR):
	@mkdir -p $@

# Re-validate every compiled module with rocqchk.
check: clean build
	@for f in $(VFILES); do \
	  vo="$(BUILD_DIR)/$$(basename $$f .v).vo"; \
	  echo "  → rocq check $$vo"; \
	  $(ROCQ) check $(LOAD_FLAGS) "$$vo" || exit 1; \
	done

# Watch loop. Requires entr (in nixpkgs) or falls back to a poll loop.
watch:
	@if command -v entr >/dev/null; then \
	  find . -name '*.v' -not -path './$(BUILD_DIR)/*' | entr -c $(MAKE) build; \
	else \
	  while true; do $(MAKE) -s build; sleep 2; done; \
	fi

clean:
	@rm -rf $(BUILD_DIR)
	@find . -maxdepth 1 -type f \( \
	    -name '*.vo' -o -name '*.vok' -o -name '*.vos' \
	    -o -name '*.glob' -o -name '.*.aux' -o -name '*.aux' \
	  \) -delete

print-files:
	@echo "VFILES    = $(VFILES)"
	@echo "BUILD_DIR = $(BUILD_DIR)"
	@echo "LOAD_FLAGS = $(LOAD_FLAGS)"
