let
  pkgs = import <nixpkgs> { };
  hp = pkgs.haskellPackages;
in
  {
    day01 = hp.callCabal2nix "day01" ./day01 { };
    day02 = hp.callCabal2nix "day02" ./day02 { };
    day03 = hp.callCabal2nix "day03" ./day03 { };
  }
