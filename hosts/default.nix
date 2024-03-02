{ self, ... }:

{
  _file = ./default.nix;

  imports = [ ./modules ];

  flake = {
    nixosConfigurations = {
      quartz = self.lib.mkNixos {
        system = "x86_64-linux";
        extraModules = [ ./quartz ];
      };

      lapis = self.lib.mkNixos {
        #TODO: https://github.com/nix-community/NixOS-WSL
        system = "x86_64-linux";
        extraModules = [];
      };
    };
  };
}
