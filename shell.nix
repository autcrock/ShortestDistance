{ pkgs ? import <nixpkgs> {} }:
let
#   threadscope = ...
#   speedscope = ...
in
  stdenv.mkDerivation {
    name = "Shortest-Path";
    nativeBuildInputs = [
      # Haskell tools
      ghcid ghc

      # Profiling tools
#       threadscope
#       speedscope
    ];
  }

# pkgs.haskell.lib.buildStackProject {
#   name = "my-haskell-test-project";
#   src = ./.;
  # Optional: Add any additional build inputs (e.g., system libraries) here
  # buildInputs = [ pkgs.zlib ];
# }
