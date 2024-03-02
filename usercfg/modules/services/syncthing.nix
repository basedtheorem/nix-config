{ config, lib, ... }:
let
  cfg = config.presets.syncthing;
in

{
  _file = ./syncthing.nix;

  options = {
    presets.syncthing.enable = lib.mkEnableOption "Syncthing";
  };

  config = lib.mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      # tray.enable = true; # broken
    };
  };
}
