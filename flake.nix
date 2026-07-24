{
  description = "Runix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    runix-flake = {
      url = "github:n3wm1nd/runix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    universal-llm-flake = {
      url = "github:n3wm1nd/universal-llm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    runix-tools-flake = {
      url = "github:n3wm1nd/runix-tools";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, runix-flake, universal-llm-flake, runix-tools-flake, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskellPackages;

      builder = import ./builder.nix {pkgs = nixpkgs.legacyPackages.${system};};
      runix = runix-flake.packages.${system}.runix;
      universal-llm = universal-llm-flake.packages.${system}.universal-llm;
      runix-tools = runix-tools-flake.packages.${system}.runix-tools;

      # Import templates and task-init script
      templatesModule = import ./templates.nix { inherit pkgs; };
    in
    {
      packages.${system} = {
        default = runix;
        runix = runix;
        universal-llm = universal-llm;
        runix-tools = runix-tools;
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
            haskellPackages.pandoc
            pkgs.cabal-install
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
            pandoc
            ];
        };
      };

      templates = {
        task = templatesModule.templates.task;
      };
    };
}
