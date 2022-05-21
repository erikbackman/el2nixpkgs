let
  pkgs = import ./nix/nixpkgs.nix {};
  package = import ./default.nix;
  
  haskellTooling = with pkgs;
    [ ghcid
      ghc
      cabal-install
      haskellPackages.hlint
      haskellPackages.hindent
      haskellPackages.ormolu
      haskellPackages.hasktags
      haskellPackages.hoogle
      haskell-language-server
      #(haskell-language-server.override { supportedGhcVersions = [ "902" ]; })
    ];
in
pkgs.mkShell {
  name = "haskell-dev-shell";
  inputsFrom = [ package.env ];
  buildInputs = haskellTooling ++ [pkgs.zlib];
}
