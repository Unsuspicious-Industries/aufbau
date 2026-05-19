{
  description = "Proposition 7 development environment (Rust + proof assistants + VSCodium)";

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
        };

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [
            "rust-src"
            "rust-analyzer"
            "clippy"
            "rustfmt"
          ];
        };

        rocqPkgs = pkgs.rocqPackages;


        devTools = with pkgs; [
          # Rust tools
          cargo-watch
          cargo-edit
          cargo-expand
          cargo-flamegraph
          cargo-criterion
          cargo-nextest
          cargo-deny

          valgrind
          jupyter

          # Documentation / utils
          mdbook
          mdbook-mermaid
          graphviz
          fd
          tokei
          just
          git
          git-lfs
          gh

          # Python packaging
          python3
          python3Packages.pip
          maturin

          # Linker & toolchain
          binutils
          nix-ld
          clang
          mold

          # proving
          coq
          rocqPkgs.vsrocq-language-server
          dune_3
          ocaml
          why3


          # Libraries
          openssl
          pkg-config
          zlib
        ];

        lspServers = with pkgs; [
          nil
          taplo
          yaml-language-server
          nodePackages.bash-language-server
        ];

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            rustToolchain
          ] ++ devTools ++ lspServers;

          shellHook = ''
            export RUST_BACKTRACE=1
            export RUST_LOG=info
            export CARGO_BUILD_JOBS=8
            export LEAN_PATH="${pkgs.lean4}/lib/lean"

            # Linker fixes for NixOS
            export LD=$(which clang)
            export RUSTFLAGS="-C linker=clang -C link-arg=-fuse-ld=mold"
            export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}:${pkgs.zlib}/lib:$LD_LIBRARY_PATH"

            # OpenSSL / pkg-config for build scripts
            export OPENSSL_DIR="${pkgs.openssl.dev}"
            export OPENSSL_LIB_DIR="${pkgs.openssl.out}/lib"
            export OPENSSL_INCLUDE_DIR="${pkgs.openssl.dev}/include"
            export PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
            export LIBRARY_PATH="${pkgs.openssl.out}/lib:$LIBRARY_PATH"
            export PYO3_PYTHON="${pkgs.python3}/bin/python3"

            # Virtual environment for maturin develop
            VENV=".venv"
            if [ ! -d "$VENV" ]; then
              echo "Creating Python virtual environment at $VENV ..."
              "${pkgs.python3}/bin/python3" -m venv "$VENV"
            fi
            export VIRTUAL_ENV="$PWD/$VENV"
            export PATH="$VENV/bin:$PATH"
          '';
        };

        formatter = pkgs.alejandra;
      }
    );
}
