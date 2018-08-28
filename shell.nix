{compiler ? "default"}:
let
  days = (import ./build.nix {inherit compiler;}).days;
  pkgs = (import ./build.nix {inherit compiler;}).pkgs;
  mkShell = name: value: value.env.overrideAttrs (old: rec {
    nativeBuildInputs = old.nativeBuildInputs ++ [
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.cabal-install
    ];
    shellHook = old.shellHook + "\ncd " + name;
  });
in
  pkgs.lib.attrsets.mapAttrs mkShell days
