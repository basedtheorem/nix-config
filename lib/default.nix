{ lib, ... }:

{
  _file = ./default.nix;

  flake.lib = rec {
    emacs.generatePackage = import ./genEmacsPackage.nix;

    flattenAttrs = attr: lib.concatMapAttrs (n: v:
      if lib.hasSuffix ".nix" n
      then { ${n} = v; }
      else {}
    ) attr;

    # Imports every .nix from input path recursively.
    readNixFilesRec = path: (lib.concatMapAttrs (n: v:
      if v == "directory"
      then readNixFilesRec (path + "/${n}")
      else
        if lib.hasSuffix ".nix" n
        then { ${lib.removeSuffix ".nix" n} = (import (path + "/${n}")); }
        else {}
    ) (builtins.readDir path));
  };
}
