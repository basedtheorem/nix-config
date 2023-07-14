{ self, inputs, ... }:

{
  flake.homeConfigurations = {
    l = inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = self.legacyPackages.x86_64-linux;
      modules = [];
    }
  };
}
