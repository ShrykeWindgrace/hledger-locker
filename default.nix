{ pkgs ? import <nixpkgs> { }, ... }:
let p = pkgs.haskell.packages.ghc8107.callCabal2nix "hledger-locker" ./. { };
in
  p // {nativeBuildInputs = [pkgs.git] ++ p.nativeBuildInputs;}
