{
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
      systems = [ "x86_64-linux" ];

      imports = [
        ./hosts
        ./home
        ./pkgs
        ./lib
      ];

      perSystem = { pkgs, system, ... }: {

        legacyPackages = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        devShells.default = pkgs.mkShell {
          name = "dotfiles";
          formatter = pkgs.alejandra;

          packages = with pkgs; [
            config.packages.repl
            alejandra
            nil # language server
          ];

          DIRENV_LOG_FORMAT = "";
        };
      };
  };


  nixConfig = {
    substituters = [
      "https://nix-community.cachix.org"
      "https://helix.cachix.org"
      "https://nix-gaming.cachix.org"
      "https://cache.privatevoid.net"
    ];

    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "helix.cachix.org-1:ejp9KQpR1FBI2onstMQ34yogDm4OgU2ru6lIwPvuCVs="
      "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
      "cache.privatevoid.net:SErQ8bvNWANeAvtsOESUwVYr2VJynfuc9JRwlzTTkVg="
    ];

    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store = true; # cli: `nix-store --optimise`
    cores = 0; # uses all cores
    trusted-users = [ "root" ];
  };
}
