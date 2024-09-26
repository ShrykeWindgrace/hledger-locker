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
      ghc = "ghc966";
      pkgs = nixpkgs.legacyPackages.${system};

      packageName = "hledger-locker";
      haskellPackages = pkgs.haskell.packages.${ghc}.extend (hself: hsuper: {
        hledger-locker = hself.callCabal2nix packageName self { };
      });
    in
    {
      packages.${packageName} =
        (haskellPackages.callCabal2nix packageName self { }).overrideAttrs(finalAttrs: previousAttrs : {
          buildPhase = ''
            export HLOCKER_BUILD_TIME_GIT_REV=${builtins.substring 0 7 (
              if self ? rev then self.rev else ""
            )}
          '' + previousAttrs.buildPhase;
        });
      packages.default = self.packages.${system}.${packageName};
      devShells.default = haskellPackages.shellFor {
        nativeBuildInputs = [
          haskellPackages.stan
          haskellPackages.stylish-haskell
          haskellPackages.cabal-install
          haskellPackages.weeder
        ];

        inputsFrom = builtins.attrValues self.packages.${system};
        packages = p: [ p.hledger-locker ];
      };
    });
}
