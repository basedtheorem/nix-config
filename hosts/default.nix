{ self, ... }:

{
  _file = ./default.nix;

  imports = [ ./modules ];

  flake = {
    nixosConfigurations = {
      quartz = self.lib.mkNixosSystem {
        system = "x86_64-linux";
        extraModules = [ ./quartz ];
      };

      lapis = self.lib.mkNixosSystem {
        #TODO: https://github.com/nix-community/NixOS-WSL
      };
    };
  };
}
