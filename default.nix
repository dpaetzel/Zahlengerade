{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (haskellPkgs: with haskellPkgs; [
          diagrams
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "Zahlengerade";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
