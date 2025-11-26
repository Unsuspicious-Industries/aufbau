{
  description = "proposition-7: Type-aware constrained decoding for LLMs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, crane }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };

        # Use Rust nightly for edition 2024
        rustToolchain = pkgs.rust-bin.nightly.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };

        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

        # Python version
        python = pkgs.python311;

        # Common build inputs
        commonBuildInputs = with pkgs; [
          openssl
          pkg-config
        ];

        # Build the p7 library first
        p7Src = ../.;
        
        # Source for the Python bindings
        pythonSrc = ./.;

        # Build the Python module using maturin
        proposition-7 = python.pkgs.buildPythonPackage {
          pname = "proposition-7";
          version = "0.1.0";
          format = "pyproject";

          src = pythonSrc;

          cargoDeps = pkgs.rustPlatform.importCargoLock {
            lockFile = ./Cargo.lock;
            outputHashes = {
              # Add any git dependencies here if needed
            };
          };

          nativeBuildInputs = with pkgs; [
            rustToolchain
            maturin
            pkg-config
          ] ++ (with pkgs.rustPlatform; [
            cargoSetupHook
            maturinBuildHook
          ]);

          buildInputs = commonBuildInputs ++ [ python ];

          # Pass the parent directory for the p7 dependency
          preBuild = ''
            export P7_SRC_DIR="${p7Src}"
          '';

          pythonImportsCheck = [ "p7_constrained" ];

          meta = with pkgs.lib; {
            description = "Type-aware constrained decoding for LLMs using Proposition 7 grammar format";
            homepage = "https://github.com/Unsuspicious-Industries/p7";
            license = licenses.mit;
          };
        };

        # Development shell
        devShell = pkgs.mkShell {
          buildInputs = commonBuildInputs ++ [
            rustToolchain
            pkgs.maturin
            python
            (python.withPackages (ps: with ps; [
              transformers
              torch
              numpy
              pytest
              ipython
            ]))
          ];

          shellHook = ''
            echo "proposition-7 development shell"
            echo "Run 'maturin develop' to build and install the Python module"
          '';
        };

      in {
        packages = {
          default = proposition-7;
          proposition-7 = proposition-7;
        };

        devShells.default = devShell;

        # Overlay for use in other flakes
        overlays.default = final: prev: {
          pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
            (python-final: python-prev: {
              proposition-7 = proposition-7;
            })
          ];
        };
      }
    );
}

