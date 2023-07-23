{ self, inputs, ... }:

let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (self) legacyPackages;
in
{
  flake.homeConfigurations = {
    l = homeManagerConfiguration {
      pkgs = legacyPackages."x86_64-linux";
      modules = [
        ./l
        # Creates a WSL2 distro tarball
        inputs.home-manager-wsl.homeModules.default
        ({ wsl.baseDistro = "void"; })
      ];
    };
  };
}
