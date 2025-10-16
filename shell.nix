{ pkgs ? import <nixpkgs> {} }:
let
#   threadscope = ...
#   speedscope = ...
in
  stdenv.mkDerivation {
    name = "Shortest-Path";
    buildInputs = [ zlib-0.7.1.1 ];

    nativeBuildInputs = [
      # Haskell tools
      ghc cabal ghcid hix haskell-language-server

      # Profiling tools
#       threadscope
#       speedscope
    ];
  }

# pkgs.haskell.lib.buildStackProject {
#   name = "my-haskell-test-project";
#   src = ./.;
##   Optional: Add any additional build inputs (e.g., system libraries) here
#   buildInputs = [ pkgs.zlib ];
# }
