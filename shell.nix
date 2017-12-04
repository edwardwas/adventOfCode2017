{compiler ? "default"}:
let
  days = (import ./build.nix {inherit compiler;}).days;
  pkgs = (import ./build.nix {inherit compiler;}).pkgs;
in
  pkgs.lib.attrsets.mapAttrs (_: value: value.env) days
