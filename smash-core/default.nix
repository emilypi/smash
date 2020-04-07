{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bifunctors, binary, deepseq, hashable
      , stdenv
      }:
      mkDerivation {
        pname = "smash";
        version = "0.1.1.0";
        src = ./.;
        libraryHaskellDepends = [
          base bifunctors binary deepseq hashable
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/emilypi/smash";
        description = "Smash products - like 'These', but with a unit!";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
