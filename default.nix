{ mkDerivation, base, binary, bytestring, criterion, deepseq
, filepath, hedgehog, hedgehog-checkers, HUnit
, optparse-applicative, parsec, primitive, smallcheck, stdenv
, tasty, tasty-discover, tasty-hedgehog, tasty-hunit
, tasty-smallcheck, temporary, text, transformers, vector
}:
mkDerivation {
  pname = "hbf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring deepseq filepath optparse-applicative parsec
    primitive text transformers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base hedgehog hedgehog-checkers HUnit smallcheck tasty
    tasty-discover tasty-hedgehog tasty-hunit tasty-smallcheck
    temporary text transformers vector
  ];
  benchmarkHaskellDepends = [
    base criterion filepath text transformers
  ];
  license = stdenv.lib.licenses.gpl3;
}
