{compiler ? "default"}:
let
  defaultCompiler = "ghc843";
  makeLocalPackage = packages: name: {
    name = name;
    value = packages.callCabal2nix name (./. + "/${name}") { };
  };
  config = {
    packageOverrides = pkgs: rec{
      haskellPackages = pkgs.haskell.packages.${if compiler == "default" then defaultCompiler else compiler}.override {
        overrides = new: old: (
        {
          mkDerivation = expr: old.mkDerivation (expr // {enableLibraryProfiling = true;});
        } 
          // builtins.listToAttrs (map (makeLocalPackage new) (allDays)));
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

  isDay = pkgs.lib.strings.hasPrefix "day";
  allDays = builtins.filter (isDay) (builtins.attrNames (builtins.readDir ./.));
in
  {
    days = builtins.listToAttrs (map (day: {name = day; value = pkgs.haskellPackages.${day};}) (allDays));
    pkgs = pkgs;
  }
