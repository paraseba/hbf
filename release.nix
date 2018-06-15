{ compiler ? "ghc822" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              orig = haskellPackagesNew.callPackage ./default.nix { };

              hbf = haskell.lib.overrideCabal orig (args: args // { doBenchmark = true; });

              # fixes a bug with multiple deriving clauses
              hindent = haskellPackagesNew.callPackage ./hindent.nix { };

              withCabal = haskell.lib.overrideCabal hbf (args: args // {testToolDepends = [pkgs.cabal-install pkgs.wget];});

              # dependency issue with either package
              hedgehog-checkers = haskell.lib.dontCheck haskellPackagesOld.hedgehog-checkers;


            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    hbf = pkgs.haskell.packages.${compiler}.hbf;
    withCabal = pkgs.haskell.packages.${compiler}.withCabal.env;
    hindent = pkgs.haskell.packages.${compiler}.hindent;
  }
