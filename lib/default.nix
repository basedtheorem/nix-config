{
  self,
  lib,
  inputs,
  withSystem,
  ...
}:

{
  _file = ./default.nix;

  flake.lib = {
    # Imports every .nix from input path recursively.
    # Returns a list of nix expressions.
    readNixExpsFrom =
      {
        path,
        excludes ? [ ],
      }:
      let
        paths = lib.filesystem.listFilesRecursive path;
        pathsToNixFiles = builtins.filter (
          path:
          lib.hasSuffix ".nix" path
          &&
            # Checks if path is in excludes list.
            !lib.lists.any (x: x == path) excludes
        ) paths;
      in
      map (file: builtins.import file) pathsToNixFiles;

    # Create home manager config.
    mkHomeCfg =
      {
        system,
        extraModules ? [ ],
      }:
      inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = withSystem system ({ pkgs, ... }: pkgs);
        modules = (builtins.attrValues self.homeModules) ++ extraModules;
        extraSpecialArgs = {
          inherit inputs self;
        };
      };

    # Create NixOS system.
    mkNixos =
      {
        system,
        extraModules ? [ ],
      }:

      inputs.nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs self;
        };
        modules = [
          { nixpkgs.hostPlatform = system; }
        ] ++ (builtins.attrValues self.nixosModules) ++ extraModules;
      };
  };
}
