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

          # Documentation / utils
          mdbook
          graphviz
          fd
          tokei
          just
          git
          git-lfs
          gh

          # Linker & toolchain
          clang
          mold
          binutils
          nix-ld

          # Libraries
          openssl
          pkg-config
          zlib

          # Editor
          pkgs.vscodium
	  zed-editor
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
            echo "Proposition 7 Dev Environment"
            echo "----------------------------"

            export RUST_BACKTRACE=1
            export RUST_LOG=info
            export CARGO_BUILD_JOBS=8
            export LEAN_PATH="${pkgs.lean4}/lib/lean"
            export COQLIB="${pkgs.coq_8_18}/lib/coq"

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

            # Show versions
            echo "Rust:     $(rustc --version)"
            echo "Cargo:    $(cargo --version)"
            echo "Python:   $(python --version)"
            echo "VSCodium available: codium ."
            echo ""
            echo "Quick commands:"
            echo "  cargo watch -x check    # Auto-check on save"
            echo "  cargo nextest run       # Run tests fast"
            echo "  cargo flamegraph        # Profile with flamegraph"
          '';
        };

        formatter = pkgs.alejandra;
      }
    );
}

