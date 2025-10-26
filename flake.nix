{
  description = "Runix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskellPackages;

      # Override cabal-install to version 3.14.2.0 for HLS compatibility
      # Use callCabal2nix to automatically generate proper derivations
      cabalSrc = pkgs.fetchFromGitHub {
        owner = "haskell";
        repo = "cabal";
        rev = "cabal-install-v3.14.2.0";
        hash = "sha256-0hUwkTYaW2grVTJU9VJtY7iBbbWBsOrCPHkMEy0hKUM=";
      };

      haskellPackages314 = haskellPackages.override {
        overrides = self: super: {
          Cabal-syntax = pkgs.haskell.lib.dontCheck (self.callCabal2nix "Cabal-syntax" "${cabalSrc}/Cabal-syntax" {});
          Cabal = pkgs.haskell.lib.dontCheck (self.callCabal2nix "Cabal" "${cabalSrc}/Cabal" {});
          Cabal-described = pkgs.haskell.lib.dontCheck (self.callCabal2nix "Cabal-described" "${cabalSrc}/Cabal-described" {});
          Cabal-QuickCheck = pkgs.haskell.lib.dontCheck (self.callCabal2nix "Cabal-QuickCheck" "${cabalSrc}/Cabal-QuickCheck" {});
          Cabal-tests = pkgs.haskell.lib.dontCheck (self.callCabal2nix "Cabal-tests" "${cabalSrc}/Cabal-tests" {});
          Cabal-tree-diff = pkgs.haskell.lib.dontCheck (self.callCabal2nix "Cabal-tree-diff" "${cabalSrc}/Cabal-tree-diff" {});
          cabal-install-solver = pkgs.haskell.lib.dontCheck (self.callCabal2nix "cabal-install-solver" "${cabalSrc}/cabal-install-solver" {});
          cabal-install = pkgs.haskell.lib.compose.doJailbreak (pkgs.haskell.lib.dontCheck (self.callCabal2nix "cabal-install" "${cabalSrc}/cabal-install" {}));
        };
      };

      cabal-install-3-14 = haskellPackages314.cabal-install;

      builder = import ./builder.nix {pkgs = nixpkgs.legacyPackages.${system};}; 
      runix = haskellPackages.developPackage {
          name = "runix";
          root = ./runix;
        };

      # Import templates and task-init script
      templatesModule = import ./templates.nix { inherit pkgs; };
    in
    {
      packages.${system} = {
        default = runix;
        runix = runix;
        builder = builder;
        task-init = templatesModule.task-init;
      };

      devShells.${system} = {
        default = haskellPackages.shellFor {
          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.polysemy
            haskellPackages.http-conduit
            haskellPackages.aeson
            haskellPackages.autodocodec
            haskellPackages.autodocodec-schema
            haskellPackages.conduit-extra
            haskellPackages.exceptions
            cabal-install-3-14
            pkgs.cabal2nix
          ];
#          shellHook = ''
#            export GHC_PACKAGE_PATH=$NIX_GHC_LIBDIR/package.conf.d
#          '';
          withHoogle = true;
          packages = p : with p; [
            polysemy
            aeson
            http-conduit
            autodocodec
            autodocodec-schema
            ];
        };
      };

      templates = {
        task = templatesModule.templates.task;
      };
    };
}
