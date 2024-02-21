{ lib, ... }:

{
  _file = ./default.nix;
  flake.lib = {
    emacs.generatePackage = import ./genEmacsPackage.nix;
    filterModules = import ./filterModules.nix { inherit lib; };
  };
}
