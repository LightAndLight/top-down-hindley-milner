{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "ghc924";
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.packages.${ghcVersion}.ghc
            cabal-install
            haskell-language-server
          ];
        };
      }
    );
}
