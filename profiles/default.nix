{
  self,
  nixpkgs,
  inputs,
  ...
}: let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in {
  flake.homeConfigurations = {
    # Main
    l = homeManagerConfiguration {
      pkgs = self.legacyPackages."x86_64-linux";
      extraSpecialArgs = {inherit inputs;};
      modules = [
        ./l
      ];
    };
  };
}
