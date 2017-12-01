let
  pkgs = import <nixpkgs> { };
in
  {
    day01 = pkgs.haskellPackages.callPackage ./nix/day01.nix { };
  }
