{ self, ... }:
{
  _file = ./default.nix;

  flake = {
    nixosModules = import ./nixos;
    homeModules = import ./hm;
  };
}
