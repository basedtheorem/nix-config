lib:
/*
  Imports every module in the current directory
  and assigns them to their file names.
*/
{
  filterModules = path:
    let
      inherit (lib.attrsets) mergeAttrsList;

      files =
        lib.filter (a: (baseNameOf a) != "default.nix")
          (lib.filesystem.listFilesRecursive path);

      # [ "dir/gnome.nix" ] => [ "gnome" ]
      names = lib.forEach files (e: lib.removeSuffix ".nix" (baseNameOf e));

      # [ "dir/gnome.nix" ] => [ <lambda> ]
      modules = lib.forEach files (e: import e);
    in
    mergeAttrsList (lib.zipListsWith (a: b: { "${a}" = b; }) names modules);
}
