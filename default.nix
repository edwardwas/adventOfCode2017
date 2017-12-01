let
  pkgs = import <nixpkgs> { };
  hp = pkgs.haskellPackages;
in
  {
    day01 = hp.callPackage ./nix/day01.nix { };
  }
