{
  flake.nixosModules = {
    presets = {
      imports = [
        ./gnome.nix
        ./nix.nix
      ];
    };
  };
}
