{
  description = "Parse elisp package configs into a list of nixpkgs";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
    let 
        pkgs = import nixpkgs {inherit system;};
        el2nixpkgs = pkgs.haskell.packages.ghc902.callCabal2nix "el2nixpkgs" ./. {};
    in {

      defaultPackage = el2nixpkgs;

      packages = flake-utils.lib.flattenTree {
        inherit el2nixpkgs;
      };

      devShell = import ./shell.nix { inherit el2nixpkgs; pkgs = pkgs; };

    });
}
