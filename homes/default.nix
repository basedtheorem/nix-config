{ self, ... }:

{
  _file = ./default.nix;

  flake = {
    homeConfigurations = {
      l = self.lib.mkHomeCfg{
        system = "x86_64-linux";
        extraModules = [ ./l ];
      };
    };
  };
}
