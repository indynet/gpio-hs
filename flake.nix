{
  description = "gpio-hs";

  inputs      = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs     = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs   = import nixpkgs { inherit system; };
      hpkgs  = pkgs.haskell.packages.ghc912;
      build  = [
        pkgs.haskell.compiler.ghc912
        hpkgs.cabal-install
        pkgs.hpack
        pkgs.zlib
      ];
    in {
      packages.${system}.default = hpkgs.developPackage {
        modifier = drv: pkgs.haskell.lib.addBuildTools drv build;
        root     = self;
      };
    };
}
