{ compiler ? "ghc964" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "hledger-locker" =
        (hself.callCabal2nix "hledger-locker" (gitignore ./.) { }).overrideAttrs (
          old: rec {
            nativeBuildInputs = [ pkgs.git ] ++ old.nativeBuildInputs;
          }
        );
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."hledger-locker"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      #pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.stylish-haskell
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      #pkgs.haskellPackages.stan  # marked as broken and allowing broken packages does not fix the issue here
      #pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."hledger-locker");

  #docker = pkgs.dockerTools.buildImage {
  #name = "hledger-locker";
  #config.Cmd = [ "${exe}/bin/hledger-locker" ];
  #};
in
{
  inherit shell;
  inherit exe;
  #inherit docker;
  inherit myHaskellPackages;
  "hledger-locker" = myHaskellPackages."hledger-locker";
}
