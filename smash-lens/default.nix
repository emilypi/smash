{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lens, smash, stdenv }:
      mkDerivation {
        pname = "smash-lens";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base lens smash ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/emilypi/smash";
        description = "Optics for the `smash` library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
