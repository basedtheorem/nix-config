{
  description = "?";

  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-23.05";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    unstable-small.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    master.url = "github:nixos/nixpkgs/master";

    nixpkgs.follows = "unstable-small";

    parts.url = "github:hercules-ci/flake-parts";
    parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";
    nur.follows = "nixpkgs";

    spicetify-nix.url = "github:the-argus/spicetify-nix";
    spicetify-nix.inputs.nixpkgs.follows = "nixpkgs";

    chillax-discord-theme.url = "github:warrayquipsome/Chillax";
    chillax-discord-theme.flake = false;

    fish-tide.url = "github:IlanCosman/tide";
    fish-tide.flake = false;

    kitty-grab.url = "github:yurikhan/kitty_grab";
    kitty-grab.flake = false;

    flatpak.url = "github:GermanBread/declarative-flatpak/stable";
    flatpak.inputs.nixpkgs.follows = "nixpkgs";

    mpv.url = "github:mpv-player/mpv";
    mpv.flake = false;

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    indent-bars.url = "github:jdtsmith/indent-bars";
    indent-bars.flake = false;
  };

  outputs =
    inputs:
    inputs.parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      debug = true;

      imports = [
        ./hosts
        ./usercfg
        ./packages
        ./overlays
        ./lib
      ];

      perSystem =
        {
          pkgs,
          system,
          lib,
          ...
        }:
        {
          # `nix fmt **.nix`
          formatter = pkgs.nixfmt-rfc-style;

          devShells.default = pkgs.mkShell rec {
            name = "Nome Development Environment";

            packages = builtins.attrValues { inherit (pkgs) nil nixfmt-rfc-style; };

            shellHook = ''
              echo
              echo Packages loaded:
              echo ' -' ${builtins.concatStringsSep "\necho ' - '" (lib.forEach packages lib.getName)}
            '';

            DIRENV_LOG_FORMAT = "";
          };
        };
    };
}
