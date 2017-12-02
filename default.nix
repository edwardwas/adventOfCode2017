let
  pkgs = import <nixpkgs> { };
  hp = pkgs.haskellPackages;
in
  {
    day01 = hp.callPackage ./nix/day01.nix { };
    day02 = hp.callPackage ./nix/day02.nix { };
  }
