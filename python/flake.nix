{
  description = "Proposition 7 - Type-aware constrained decoding for LLMs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
          # Explicitly disable CUDA to use CPU-only packages
          config = {
            allowUnfree = true;
            cudaSupport = false;
          };
        };

        # Use stable Rust toolchain
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };

        # Python with pre-built packages (no compilation)
        python = pkgs.python312;
        
        # Python environment with all dependencies
        pythonEnv = python.withPackages (ps: with ps; [
          # Build tools
          pip
          setuptools
          wheel
          
          # Development
          pytest
          numpy
          accelerate
          ipykernel

          # Optional: transformers integration
          # Note: Using CPU-only torch from nixpkgs (pre-built)
          torch  # CPU version from nixpkgs
          transformers
          tokenizers
          huggingface-hub
          safetensors
          
          # Other useful deps
          tqdm
          pyyaml
          regex
        ]);

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Rust toolchain
            rustToolchain
            pkgs.cargo
            pkgs.rustc
            
            # Python with all packages
            pythonEnv
            
            # Maturin for building the extension
            pkgs.maturin
            
            # Build essentials
            pkgs.pkg-config
            pkgs.openssl
            
            # For linking
            pkgs.stdenv.cc.cc.lib
          ];

          shellHook = ''
            # Set library path for linking
            export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}/lib:$LD_LIBRARY_PATH"
            
            # Point maturin to the right Python interpreter  
            export VIRTUAL_ENV="${pythonEnv}"
            export PYO3_PYTHON="${pythonEnv}/bin/python"
            
            # Tell cargo this is NOT a cross-compile
            export CARGO_BUILD_TARGET="x86_64-unknown-linux-gnu"
            export CARGO_TARGET_DIR="$PWD/target"
            
            # Unset cross-compilation variables that confuse maturin/pyo3
            unset CC_FOR_TARGET
            unset CXX_FOR_TARGET
            unset AR_FOR_TARGET
            unset NIX_CC_FOR_TARGET
            unset NIX_BINTOOLS_FOR_TARGET
            unset NIX_LDFLAGS_FOR_TARGET
            unset NIX_CFLAGS_COMPILE_FOR_TARGET
            
            echo "╔══════════════════════════════════════════════════════════════╗"
            echo "║                     PROPOSITION 7                            ║"
            echo "║         Type-aware Constrained LLM Generation                ║"
            echo "╠══════════════════════════════════════════════════════════════╣"
            echo "║  Python: $(python --version 2>&1 | cut -d' ' -f2)                                          ║"
            echo "║  Rust: $(rustc --version | cut -d' ' -f2)                                            ║"
            echo "║  CUDA: disabled (CPU-only torch)                             ║"
            echo "╚══════════════════════════════════════════════════════════════╝"
            echo ""
            echo "Quick start:"
            echo "  maturin develop --skip-install  # Build extension in-place"
            echo "  python examples/gpt2.py         # Run demo"
            echo ""
          '';

          # Prevent Nix from trying to build CUDA packages
          CUDA_VISIBLE_DEVICES = "";
        };

        # Package for building the wheel
        packages.default = pkgs.python312Packages.buildPythonPackage {
          pname = "proposition-7";
          version = "0.1.0";
          format = "pyproject";
          
          src = ./.;
          
          nativeBuildInputs = [
            pkgs.maturin
            pkgs.cargo
            rustToolchain
          ];
          
          buildInputs = [
            pkgs.openssl
          ];
          
          propagatedBuildInputs = with python.pkgs; [
            numpy
          ];
          
          # Skip tests during build
          doCheck = false;
        };
      }
    );
}
