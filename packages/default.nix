{ ... }:
{
  _file = ./default.nix;

  perSystem =
    { system, pkgs, ... }:
    {
      packages = {
        lentenrose = pkgs.callPackage ./lentenrose.nix { };
        portmaster = pkgs.callPackage ./portmaster.nix { };
      };
    };
}
