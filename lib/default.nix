{ pkgs, lib, ... }:
{
  _file = ./default.nix;

  perSystem = { ... }: {
    options.lib = lib.mkOption {
      type = lib.types.attrsOf lib.types.unspecified;
      default = { };
    };

    config.lib = {
      filterModules = import ./filterModules.nix lib;
    };
  };
}
