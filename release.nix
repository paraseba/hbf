{ compiler ? "ghc822" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              hbf =
                haskellPackagesNew.callPackage ./default.nix { };

              withCabal = pkgs.haskell.lib.overrideCabal hbf (oldDerivation: {testToolDepends = [pkgs.cabal-install];});
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
  }
