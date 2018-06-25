{ mkDerivation, base, binary, bytestring, criterion, deepseq
, doctest, filepath, hedgehog, hedgehog-checkers, HUnit
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
  executableHaskellDepends = [
    base binary bytestring deepseq filepath optparse-applicative parsec
    primitive text transformers vector
  ];
  testHaskellDepends = [
    base binary bytestring deepseq doctest filepath hedgehog
    hedgehog-checkers HUnit optparse-applicative parsec primitive
    smallcheck tasty tasty-discover tasty-hedgehog tasty-hunit
    tasty-smallcheck temporary text transformers vector
  ];
  benchmarkHaskellDepends = [
    base binary bytestring criterion deepseq filepath
    optparse-applicative parsec primitive text transformers vector
  ];
  homepage = "https://gitlab.com/paraseba/hbf";
  description = "An optimizing Brainfuck compiler and evaluator";
  license = stdenv.lib.licenses.gpl3;
}
