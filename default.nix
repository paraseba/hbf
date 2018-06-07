{ mkDerivation, base, binary, bytestring, filepath
, optparse-applicative, parsec, primitive, stdenv, vector
}:
mkDerivation {
  pname = "hbf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring filepath optparse-applicative parsec
    primitive vector
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.gpl3;
}
