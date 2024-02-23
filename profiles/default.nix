{
  self,
  lib,
  inputs,
  ...
}:
let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in
{
  _file = ./default.nix;

  flake = {
    homeManagerModules = self.lib.readNixFilesFrom ./modules;

    homeConfigurations = {
      l = homeManagerConfiguration {
        pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs;
          inherit self;
        };

        modules = [
          ./l
          ./shared
        ] ++ builtins.attrValues self.homeManagerModules;
      };
    };
  };
}
