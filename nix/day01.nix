{ mkDerivation, base, file-embed, stdenv }:
mkDerivation {
  pname = "day01";
  version = "0.1.0.0";
  src = ../day01;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base file-embed ];
  license = stdenv.lib.licenses.mit;
}
