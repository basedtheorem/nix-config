{ self, inputs, ... }:
let
  inherit (inputs.nixpkgs.lib) nixosSystem;
in
{
  _file = ./default.nix;

  flake = {
    nixosModules = self.lib.readNixFilesFrom ./modules;

    nixosConfigurations = {
      quartz = nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit self inputs;
        };
        modules = [ ./quartz ] ++ builtins.attrValues self.nixosModules;
      };
    };
  };
}
