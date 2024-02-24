{ pkgs, ... }:

{
  _file = ./advcp.nix;

  config = {
    home.packages = [ pkgs.advcpmv ];

    home.shellAliases = {
      cp = "advcp -g";
      mv = "advmv -g";
    };
  };
}
