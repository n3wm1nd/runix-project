{
  description = "sse-parser";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskellPackages;

      sse-parser = haskellPackages.developPackage {
        name = "sse-parser";
        root = ./.;
      };
    in
    {
      packages.${system} = {
        default = sse-parser;
        sse-parser = sse-parser;
      };

      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ sse-parser ];
        buildInputs = [ pkgs.cabal-install ];
      };
    };
}
