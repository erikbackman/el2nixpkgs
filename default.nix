let pkgs = import ./nix/nixpkgs.nix {};
in pkgs.haskell.packages.ghc902.callCabal2nix "haskell-temp" ./. { }
