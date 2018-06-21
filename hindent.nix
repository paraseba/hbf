{ mkDerivation, base, bytestring, Cabal, containers, criterion
, deepseq, descriptive, Diff, directory, exceptions, fetchgit
, filepath, ghc-prim, haskell-src-exts, hspec, monad-loops, mtl
, path, path-io, stdenv, text, transformers, unix-compat
, utf8-string, yaml
}:
mkDerivation {
  pname = "hindent";
  version = "5.2.5";
  src = fetchgit {
    url = "https://github.com/paraseba/hindent";
    sha256 = "10z2wgad4rkxqcjc2znq249g0xv13j63zvc9qd2w3xf66rxqnp1s";
    rev = "08e8facac82ca14e731fd7202387ed6b0fd3d278";
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring Cabal containers directory exceptions filepath
    haskell-src-exts monad-loops mtl text transformers utf8-string yaml
  ];
  executableHaskellDepends = [
    base bytestring deepseq descriptive directory exceptions ghc-prim
    haskell-src-exts path path-io text transformers unix-compat
    utf8-string yaml
  ];
  testHaskellDepends = [
    base bytestring deepseq Diff directory exceptions haskell-src-exts
    hspec monad-loops mtl utf8-string
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion deepseq directory exceptions ghc-prim
    haskell-src-exts utf8-string
  ];
  homepage = "https://github.com/commercialhaskell/hindent";
  description = "Extensible Haskell pretty printer";
  license = stdenv.lib.licenses.bsd3;
}
