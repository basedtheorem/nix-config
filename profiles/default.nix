{ self, inputs, ... }:

let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (self) legacyPackages;
in
{
  flake.homeConfigurations = {
    # Main
    l = homeManagerConfiguration {
      pkgs = legacyPackages."x86_64-linux";
      modules = [
        ./l
        # Allows creating WSL distros.
        inputs.home-manager-wsl.homeModules.default
        # `nix build ~/dots#homeConfigurations.l.config.wsl.tarball`
        ({ wsl.baseDistro = "void"; })
      ];
    };

    # CLI only for use on non-nixos devices
    q = homeManagerConfiguration {
      pkgs = legacyPackages."x86_64-linux";
      modules = [
        ./q
      ];
    };
  };
}
