{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "day01";
  version = "0.1.0.0";
  src = ./day01;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
