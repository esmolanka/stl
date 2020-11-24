{ compiler ? "ghc884" }:

let
  nixpkgs = import ./. { inherit compiler; };
in

nixpkgs.haskellPackages.shellFor {
  packages = p: with p; [
    stl
    stl-runtime
  ];
  buildInputs = [
    nixpkgs.haskellPackages.cabal-install
  ];
}
