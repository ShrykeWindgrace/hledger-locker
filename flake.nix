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

      haskellPackages = pkgs.haskell.packages.${ghc};
      packageName = "hledger-locker";
    in
    {
      packages.${packageName} =
        (haskellPackages.callCabal2nix packageName self rec {
          # Dependency overrides go here
        }).overrideAttrs (
          old: rec {
            nativeBuildInputs = [ pkgs.git ] ++ old.nativeBuildInputs;
          }
        );
      packages.default = self.packages.${system}.${packageName};
      devShells.default = pkgs.mkShell {
        buildInputs = [ ];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
    });
}
