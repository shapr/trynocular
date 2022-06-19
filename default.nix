{ compiler ? "ghc922" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "takedouble" = hself.callCabal2nix "takedouble" (gitignore ./.) { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [ p."takedouble" ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hasktags
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."takedouble");

  docker = pkgs.dockerTools.buildImage {
    name = "takedouble";
    config.Cmd = [ "${exe}/bin/takedouble" ];
  };
in {
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "takedouble" = myHaskellPackages."takedouble";
}
