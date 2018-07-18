{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  drv = import ./default.nix { inherit nixpkgs compiler; };
  drvWithTools = (pkgs.haskell.lib.addBuildDepends drv [
    pkgs.cabal-install
    pkgs.haskellPackages.stylish-haskell
    pkgs.haskellPackages.stack
    pkgs.haskellPackages.hoogle
#    pkgs.haskellPackages.ghcid
#    pkgs.haskellPackages.hdevtools
#    pkgs.haskellPackages.hlint
  ]);
in
  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
