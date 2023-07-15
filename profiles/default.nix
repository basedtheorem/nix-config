{ self, inputs, ... }:

let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (self) legacyPackages;
in
{
  flake.homeConfigurations = {
    l = homeManagerConfiguration {
      pkgs = legacyPackages."x86_64-linux";
      modules = [ ./l ];
    };
  };
}
