{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.presets.advcp;
in
{
  _file = ./advcp.nix;

  options = {
    presets.advcp.enable = lib.mkEnableOption "Advanced CP & MV";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.advcpmv ];

    home.shellAliases = {
      cp = "advcp -g";
      mv = "advmv -g";
    };
  };
}
