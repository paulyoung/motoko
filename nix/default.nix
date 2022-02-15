{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.19/nix/sources.nix;
    sha256 = "1n92ka2rkdiib6ian6jh2b7fwvklnnwlp5yy5bv6ywm7m1y5hyfl";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  bootstrap-pkgs = import nixpkgs_src {
    system = builtins.currentSystem;
  };

  # dump nixpkgs patches here
  nixpkgs-patches = [];

  nixpkgs-patched =
    if nixpkgs-patches == []
    then nixpkgs_src
    else
      let
        bootstrap-pkgs = import nixpkgs_src {
          system = builtins.currentSystem;
        };
      in bootstrap-pkgs.applyPatches {
        name = "nixpkgs-patched";
        src = nixpkgs_src;
        patches = nixpkgs-patches;
      };

  pkgs =
    import nixpkgs-patched {
      inherit system;
      overlays = [
        # add nix/sources.json
        (self: super: {
           sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };
        })

        # Selecting the ocaml version
        # (self: super: { ocamlPackages = super.ocamlPackages; })

        (
          self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // {
              obelisk = import ./ocaml-obelisk.nix {
                inherit (self) lib fetchFromGitHub ocaml dune_2;
                inherit (self) ocamlPackages;
                inherit (self.stdenv) mkDerivation;
              };
            };
          }
        )

        # Rust nightly
        (self: super: let
          moz_overlay = import self.sources.nixpkgs-mozilla self super;
          rust-channel = moz_overlay.rustChannelOf { date = "2021-12-02"; channel = "nightly"; };
        in rec {
          rustc-nightly = rust-channel.rust.override {
            targets = [
               "wasm32-unknown-unknown"
               "wasm32-unknown-emscripten"
               "wasm32-wasi"
               "i686-unknown-linux-gnu"
            ];
            extensions = ["rust-src"];
          };
          cargo-nightly = rustc-nightly;
          rustPlatform-nightly = pkgs.makeRustPlatform {
            rustc = rustc-nightly;
            cargo = cargo-nightly;
          };
        })

        # wasm-profiler
        (self: super: import ./wasm-profiler.nix self)

        # drun
        (self: super: import ./drun.nix self)

        # to allow picking up more recent Haskell packages from Hackage
        # don't use `fetchFromGitHub` here as we really need an intact tarball
        (self: super: {
          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/d859530d8342c52d09a73d1d125c144725b5945d.tar.gz";
            sha256 = "0gjahsqqq99dc4bjcx9p3z8adpwy51w3mzrf57nib856jlvlfmv5";
          };
        })

        (self: super: {
          # https://github.com/nmattia/niv/issues/332#issuecomment-958449218
          niv = self.haskell.lib.compose.overrideCabal (drv: { enableSeparateBinOutput = false; }) super.haskellPackages.niv;

          # https://github.com/NixOS/nixpkgs/pull/109571
          wasmtime = with self; rustPlatform.buildRustPackage rec {
            pname = "wasmtime";
            version = "0.32.0";

            src = fetchFromGitHub {
              owner = "bytecodealliance";
              repo = pname;
              rev = "v${version}";
              sha256 = "sha256-iko2G2cUIQYv7Sia8fLtb7d6XCbpOKz31ST62eE19B0";
              fetchSubmodules = true;
            };

            cargoSha256 = "sha256-z8x004BbRWi9cRf2I27uiFuu2Jnr1Z3Ey992S5hdyNs";

            nativeBuildInputs = [ python cmake clang ];
            buildInputs = [ llvmPackages.libclang ] ++
            lib.optionals stdenv.isDarwin [ darwin.apple_sdk.frameworks.Security ];
            LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";

            configurePhase = ''
              export HOME=$TMP;
            '';

            doCheck = true;

            meta = with lib; {
              description = "Standalone JIT-style runtime for WebAssembly, using Cranelift";
              homepage = "https://github.com/bytecodealliance/wasmtime";
              license = licenses.asl20;
              maintainers = [ maintainers.matthewbauer ];
              platforms = platforms.unix;
            };
          };
        })

      ];
    };
in
pkgs
