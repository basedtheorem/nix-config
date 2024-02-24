{ self, ... }:

{
  _file = ./default.nix;

  flake = {
    nixosConfigurations = {
      quartz = self.lib.mkNixosSystem {
        system = "x86_64-linux";
        extraModules = [ ./quartz ];
      };
    };
  };
}
