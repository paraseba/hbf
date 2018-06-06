{ mkDerivation, base, binary, bytestring, parsec, stdenv, vector }:
mkDerivation {
  pname = "hbf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base binary bytestring parsec vector ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.gpl3;
}
