{ pkgs ? import <nixpkgs> { }, ... }:
let p = pkgs.haskell.packages.ghc8107.callCabal2nix "hledger-locker" ./. { };
in
  p.overrideAttrs (
    oldAttrs: rec {
      nativeBuildInputs = [pkgs.git] ++ oldAttrs.nativeBuildInputs;
    }
  )
