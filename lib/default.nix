inputs: {
  _file = ./default.nix;

  filterModules = import ./filterModules.nix inputs.lib;
}
