{
  description = "?";

  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-23.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    master.url = "github:nixos/nixpkgs/master";

    nixpkgs.follows = "unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs:
    inputs.parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      systems = [ "x86_64-linux" ];

      flake = {
        nixosModules = import ./modules/nixos inputs;
        homeManagerModules = import ./modules/home-manager inputs;
      };

      imports = [
        ./hosts
        ./profiles
        ./packages
        ./lib
      ];

      perSystem = { pkgs, system, ... }: {
        legacyPackages = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        devShells.default = pkgs.mkShell {
          name = "dotfiles devenv";
          formatter = pkgs.alejandra;

          packages = with pkgs; [
            alejandra
            nil # language server
          ];

          DIRENV_LOG_FORMAT = "";
        };
      };
  };
}
