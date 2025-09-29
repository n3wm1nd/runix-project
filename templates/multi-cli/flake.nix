{
  description = "runix-__appName__-multi-cli";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    runix = {
      type = "path";
      path = "/home/newmind/git/runix/";
    };
    # Add your task inputs here, e.g.:
    # taskOne = { type = "path"; path = "__taskOnePath__"; };
    # taskTwo = { type = "path"; path = "__taskTwoPath__"; };
  };

  outputs = { self, nixpkgs, runix, ... }@inputs:
    let
      system = "x86_64-linux";
      builder = runix.outputs.packages.${system}.builder;
      pkgs = nixpkgs.legacyPackages.${system};

      # Extract task inputs (all inputs except nixpkgs, runix, and self)
      taskInputs = builtins.removeAttrs inputs ["nixpkgs" "runix" "self"];

      definition = {mkDerivation, runix, polysemy, ...}@args : mkDerivation {
        pname = "runix-__appName__-multi-cli";
        src = self;
        version = "0.1.0.0";
        license = "GPL";
        isExecutable = true;
        executableHaskellDepends = [runix polysemy] ++
          (builtins.attrValues (builtins.intersectAttrs taskInputs args));
      };

      inherit (pkgs.haskellPackages) polysemy;
      maketrusted = builder.maketrusted;

      # Convert task inputs to packages
      taskPackages = builtins.mapAttrs
        (name: input: input.outputs.packages.${system}.default)
        taskInputs;

      result = builder.buildTask definition
        ({
          runix = runix.outputs.packages.${system}.default;
          polysemy = maketrusted polysemy;
        } // taskPackages);
    in
    {
      packages.${system} = {
        default = result;
      };
    };
}