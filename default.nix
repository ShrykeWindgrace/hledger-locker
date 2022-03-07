{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.haskell.packages.ghc8107.callCabal2nix "hledger-locker" ./. { }
