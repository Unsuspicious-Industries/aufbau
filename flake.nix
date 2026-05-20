{
  description = "aufbau: Semantic prefix grammars development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowUnfree = true;
        };

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = ["rust-src" "rust-analyzer" "clippy" "rustfmt"];
        };

        rocqPkgs = pkgs.rocqPackages;

        devTools = with pkgs; [
          cargo-watch cargo-edit cargo-expand
          cargo-flamegraph cargo-criterion cargo-nextest cargo-deny
          valgrind jupyter
          mdbook mdbook-mermaid graphviz fd tokei just git git-lfs gh
          python3 python3Packages.pip maturin
          binutils nix-ld clang mold
          rocqPkgs.rocq-core rocqPkgs.stdlib rocqPkgs.vsrocq-language-server
          dune_3 ocaml why3 why3find
          openssl pkg-config zlib
          nil taplo yaml-language-server bash-language-server
        ];

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [ rustToolchain ] ++ devTools;

          shellHook = ''
            export RUST_BACKTRACE=1 RUST_LOG=info CARGO_BUILD_JOBS=8
            export LEAN_PATH="${pkgs.lean4}/lib/lean"

            export ROCQPATH="${rocqPkgs.stdlib}/lib/coq/${rocqPkgs.rocq-core.rocq-version}/user-contrib"
            export COQPATH="$ROCQPATH"

            export LD=$(which clang)
            export RUSTFLAGS="-C linker=clang -C link-arg=-fuse-ld=mold"
            export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}:${pkgs.zlib}/lib:$LD_LIBRARY_PATH"

            export OPENSSL_DIR="${pkgs.openssl.dev}"
            export OPENSSL_LIB_DIR="${pkgs.openssl.out}/lib"
            export OPENSSL_INCLUDE_DIR="${pkgs.openssl.dev}/include"
            export PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
            export LIBRARY_PATH="${pkgs.openssl.out}/lib:$LIBRARY_PATH"
            export PYO3_PYTHON="${pkgs.python3}/bin/python3"

            VENV=".venv"
            if [ ! -d "$VENV" ]; then
              echo "Creating Python virtual environment at $VENV ..."
              "${pkgs.python3}/bin/python3" -m venv "$VENV"
            fi
            export VIRTUAL_ENV="$PWD/$VENV"
            export PATH="$VENV/bin:$PATH"

            echo "  → cargo build"
            echo ""
          '';
        };

        formatter = pkgs.alejandra;
      }
    );
}
