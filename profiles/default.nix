{
  self,
  inputs,
  ...
}: let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (self) legacyPackages;
in {
  flake.homeConfigurations = {
    # Main
    l = homeManagerConfiguration {
      pkgs = legacyPackages."x86_64-linux";
      extraSpecialArgs = {inherit inputs;};
      modules = [./l];
    };
}
