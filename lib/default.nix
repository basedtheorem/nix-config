inputs: {
  _file = ./default.nix;

  filterModules = import ./filterModules.nix inputs.lib;
  emacs.genPackage = import ./genEmacsPackage.nix;
}
