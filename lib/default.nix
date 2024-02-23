{ lib, ... }:

{
  _file = ./default.nix;

  flake.lib = {
    # Imports every .nix from input path recursively.
    readNixFilesFrom =
      path:
      let
        lsFilesRec = lib.filesystem.listFilesRecursive;
        strFiles = lib.forEach (lsFilesRec path) (p: builtins.toString p);
        strNixFiles = builtins.filter (str: lib.hasSuffix "nix" str) strFiles;
        modules = lib.genAttrs (strNixFiles) (file: builtins.import file);
      in
      # lib.mapAttrs'
      #   (
      #     name: value:
      #     lib.nameValuePair
      #       (builtins.replaceStrings
      #         [
      #           "/"
      #           ".nix"
      #         ]
      #         [
      #           "-"
      #           ""
      #         ]
      #         name
      #       )
      #       value
      #   )
      modules;
  };
}
