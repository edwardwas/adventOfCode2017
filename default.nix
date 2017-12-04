{compiler ? "default"}:
(import ./build.nix {inherit compiler; }).days
