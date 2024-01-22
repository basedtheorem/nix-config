{
  description = "?";

  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-23.05";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    master.url = "github:nixos/nixpkgs/master";

    nixpkgs.follows = "unstable";

    parts.url = "github:hercules-ci/flake-parts";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";
    nur.follows = "nixpkgs";

    spicetify-nix.url = "github:the-argus/spicetify-nix";

    chillax-discord-theme.url = "github:warrayquipsome/Chillax";
    chillax-discord-theme.flake = false;

    fish-peco-todoist.url = "github:ka2n/fish-peco_todoist";
    fish-peco-todoist.flake = false;
    fish-tide.url = "github:IlanCosman/tide";
    fish-tide.flake = false;
  };

  outputs = inputs:
    inputs.parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      debug = true;

      flake = {
        nixosModules = import ./modules/nixos inputs;
        homeManagerModules = import ./modules/home-manager inputs;
        lib = import ./lib inputs;
      };

      imports = [./hosts ./profiles ./packages];

      perSystem = {
        pkgs,
        system,
        lib,
        ...
      }: {
        legacyPackages = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
          config.permittedInsecurePackages = [
            "electron-25.9.0"
          ];
        };

        devShells.default = pkgs.mkShell rec {
          name = "dotfiles devenv";
          formatter = pkgs.alejandra;

          packages = with pkgs; [
            alejandra # formatter
            nil # language server
          ];

          shellHook = ''
            echo
            echo Entering NixOS config environment...
            echo
            echo Packages: ${
              builtins.concatStringsSep ", " (lib.forEach packages lib.getName)
            }
          '';

          DIRENV_LOG_FORMAT = "";
        };
      };
    };
}
