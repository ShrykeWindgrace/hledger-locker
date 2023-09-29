{ compiler ? "ghc946" }:
# current nipkgs-unstable is stuck with hledger-lib-1.27.1, which sets the bound basae < 4.17
# the latter corresponds to ghc-9.4.1
# hence we are stuck with 9.2.7 for the time being

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
