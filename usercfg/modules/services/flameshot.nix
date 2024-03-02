{ config, lib, ... }:
let
  cfg = config.presets.flameshot;
in

{
  _file = ./flameshot.nix;

  options = {
    presets.flameshot.enable = lib.mkEnableOption "Flameshot (screenshotting tool)";
  };

  config = lib.mkIf cfg.enable {
    services.flameshot = {
      enable = true;
    };
  };
}
