{ self, ... }:

{
  _file = ./default.nix;

  imports = [ ./modules ];

  flake = {
    homeConfigurations = {
      l = self.lib.mkHomeCfg {
        system = "x86_64-linux";
        extraModules = [ ./l.nix ];
      };
    };
  };
}
