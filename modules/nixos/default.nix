{ lib, ... }:
/*
Imports every module in the current directory
and assigns them to their file names.
*/
let
  # Filter for `*.nix` files.
  modules = lib.filterAttrs (
    name: val:
      # "regular" means normal file as opposed to (e.g.) a directory.
      if (val == "regular" && name != "default.nix") then true
      else false
  ) (builtins.readDir ./.);

in {
  lib.mapAttrs (name: val: lib.nameValuePair
    (lib.removeSuffix ".nix" name) # "module.nix" => "module"
    (import (./. + "/${name}"))
  ) modules;
}
