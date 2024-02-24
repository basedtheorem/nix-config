{ config, lib, ... }:
let
  cfg = config.presets.obs;
in
{
  options = {
    presets.obs.enable = lib.mkEnableOption "OBS";
  };

  config = lib.mkIf cfg.enable {
    programs.obs-studio = {
      enable = true;

      plugins = [ ];
    };
  };
}
