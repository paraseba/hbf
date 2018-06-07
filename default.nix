{ mkDerivation, base, binary, bytestring, filepath, hedgehog, HUnit
, optparse-applicative, parsec, primitive, stdenv, tasty
, tasty-discover, tasty-hedgehog, tasty-html, tasty-hunit, text
, vector
}:
mkDerivation {
  pname = "hbf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring filepath optparse-applicative parsec
    primitive text vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base hedgehog HUnit tasty tasty-discover tasty-hedgehog tasty-html
    tasty-hunit text
  ];
  license = stdenv.lib.licenses.gpl3;
}
