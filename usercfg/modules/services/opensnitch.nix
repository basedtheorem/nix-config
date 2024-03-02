{ config, lib, ... }:
let
  cfg = config.presets.opensnitch;
in
{
  _file = ./opensnitch.nix;

  options = {
    presets.opensnitch.enable = lib.mkEnableOption "Opensnitch (GUI)";
  };

  config = lib.mkIf cfg.enable {
    services.opensnitch-ui = {
      enable = true;
    };
  };
}
