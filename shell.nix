{ nixpkgs ? import <nixpkgs> {} }:

let

inherit (nixpkgs) pkgs;
inherit (pkgs) haskellPackages;

haskellDeps = ps: with ps; [
  base
  random
];

ghc = haskellPackages.ghcWithPackages haskellDeps;

nixPackages = [
  ghc
  haskellPackages.cabal-install
];

in pkgs.stdenv.mkDerivation {
  name = "h99";
  buildInputs = nixPackages;
}
