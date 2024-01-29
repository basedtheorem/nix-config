{
  self,
  inputs,
  withSystem,
  lib,
  ...
}: let
  inherit (inputs.nixpkgs.lib) nixosSystem;
in {
  _file = ./default.nix;

  flake.nixosConfigurations = {
    quartz = withSystem "x86_64-linux" ({self', ...}:
      nixosSystem {
        specialArgs = {inherit inputs;};
        modules =
          [
            {nixpkgs.pkgs = self'.legacyPackages;}
            ./quartz
          ]
          ++ builtins.attrValues self.nixosModules;
      });
  };
}
