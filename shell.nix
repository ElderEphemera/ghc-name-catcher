{ pkgs ? import <nixpkgs> {}
, ghcVersion ? "9_0_1"
}:

let
  ghcs = import (pkgs.fetchgit {
    url = "https://gitlab.haskell.org/bgamari/ghcs-nix";
    rev = "980f24a1a73341781d1de21151f4daaa5a66bf81";
    sha256 = "1cxiq0cj7fv6dl699idvsh7dm3ayyryrnslc5jmb126nk37n2v7i";
  }) { baseNixpkgs = pkgs; };
in pkgs.mkShell {
  packages = [
    ghcs."ghc-${ghcVersion}"
    ghcs.cabal-install
  ];
}
