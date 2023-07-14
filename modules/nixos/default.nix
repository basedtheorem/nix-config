{ lib, ... }:

let
  modules = lib.filterAttrs (
    name: val:
      if val == "regular" then (
        if name != "default.nix" then true
        else false
      )
      else false
  ) (builtins.readDir ./.);

in {
  builtins.mapAttrs (name: _: import (./. + "/${name}")) modules;
}
