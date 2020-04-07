{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, smash, stdenv
      , unordered-containers
      }:
      mkDerivation {
        pname = "smash-aeson";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ aeson base smash unordered-containers ];
        homepage = "https://github.com/emilypi/smash";
        description = "Aeson support for the smash library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
