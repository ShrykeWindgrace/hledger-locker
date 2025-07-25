{
  description = "hledger-locker";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      ghc = "ghc984";
      pkgs = nixpkgs.legacyPackages.${system};

      packageName = "hledger-locker";
      packagePostOverrides = pkg: with pkgs.haskell.lib.compose; pkgs.lib.pipe pkg [
        disableExecutableProfiling
        disableLibraryProfiling
        dontBenchmark
        dontCoverage
        dontDistribute
        dontHaddock
        dontHyperlinkSource
        doStrip
        enableDeadCodeElimination
        justStaticExecutables

        dontCheck
      ];
      haskellPackages = pkgs.haskell.packages.${ghc}.extend (hself: hsuper: {
        hledger-locker = hself.callCabal2nix packageName self { };
        stan = packagePostOverrides hsuper.stan;
        stylish-haskell = packagePostOverrides hsuper.stylish-haskell;
        weeder = packagePostOverrides hsuper.weeder;
      });
    in
    {
      packages.${packageName} =
        packagePostOverrides
          ((haskellPackages.callCabal2nix packageName self { }).overrideAttrs (finalAttrs: previousAttrs: {
            buildPhase = ''
              export HLOCKER_BUILD_TIME_GIT_REV=${builtins.substring 0 7 (
                if self ? rev then self.rev else ""
              )}
            '' + previousAttrs.buildPhase;
          }));
      packages.default = self.packages.${system}.${packageName};

      #redo this part, looks like it pull everything in the scope TODO
      devShells.default = haskellPackages.shellFor {
        nativeBuildInputs = [
          haskellPackages.stan
          haskellPackages.stylish-haskell
          #haskellPackages.cabal-install
          haskellPackages.weeder
        ];

        inputsFrom = builtins.attrValues self.packages.${system};
        packages = p: [ p.hledger-locker ];
      };
      formatter = pkgs.nixpkgs-fmt;
    });
}
