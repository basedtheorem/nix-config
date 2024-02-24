{
  _file = ./default.nix;

  flake.nixosModules = {
    presets = {
      imports = [
        ./gnome.nix
        ./nix.nix
      ];
    };
  };
}
