{ pkgs ? import <nixpkgs> {} }:
pkgs.haskell.lib.buildStackProject {
  name = "my-haskell-test-project";
  src = ./.;
  # Optional: Add any additional build inputs (e.g., system libraries) here
  # buildInputs = [ pkgs.zlib ];
}
