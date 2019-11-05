{ nixpkgs ? import ./nix/nixpkgs.nix }:

let

inherit (nixpkgs) pkgs;
inherit (pkgs) haskellPackages;

haskellDeps = ps: with ps; [
  QuickCheck
  base
  hspec
  megaparsec
  multimap
  random
  safe
  split
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
