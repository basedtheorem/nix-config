{ config, lib, pkgs, ... }:
let
  advCpMvEnabled = config.programs.advancedCpMv.enable;
  fishEnabled = config.programs.fish.enable;
in
{
  _file = ./advcp.nix;

  options.programs.advancedCpMv = {
    enable = lib.mkEnableOption "CP and MV with a progressbar";
  };

  config = lib.mkIf advCpMvEnabled {
    home.packages = [ pkgs.advcpmv ];

    programs.fish.shellAbbrs = lib.mkIf fishEnabled {
      cp = "advcp -g";
      mv = "advmv -g";
    };
  };
}
