{ reflex-platform ? import ./reflex-platform.nix
, compiler   ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    });
  };

  adjust-for-ghcjs = drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/css
      cp ./css/* $out/css

      mkdir -p $out/js
      cp $out/bin/todomvc.jsexe/all.js $out/js/todomvc.js

      cd $out/bin/todomvc.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/todomvc.min.js
      rm -Rf $out/bin/todomvc.jsexe
      rm -Rf $out/bin

      cd $out/js
      zopfli -i1000 todomvc.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  };

  adjust = drv:
    if compiler == "ghcjs"
    then adjust-for-ghcjs drv
    else drv;

  basics = pkgs.haskell.lib.overrideCabal (haskellPackages.callPackage ./todomvc.nix {}) adjust;
in
  basics
