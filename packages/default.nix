{ inputs, ... }:
{
  _file = ./default.nix;

  perSystem =
    { system, pkgs, ... }:
    {
      packages = {
        lentenrose = pkgs.callPackage ./lentenrose.nix { };
      };
    };
}
