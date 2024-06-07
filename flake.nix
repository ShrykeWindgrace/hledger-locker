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
      ghc = "ghc965";
      pkgs = nixpkgs.legacyPackages.${system};

      packageName = "hledger-locker";
      haskellPackages = pkgs.haskell.packages.${ghc}.extend (hself: hsuper: {
        hledger-locker = hself.callCabal2nix packageName self { };
      });
    in
    {
      packages.${packageName} =
        haskellPackages.callCabal2nix packageName self { };
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
