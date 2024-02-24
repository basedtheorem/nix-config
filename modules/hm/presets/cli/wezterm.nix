{ lib, config, ... }:
let
  cfg = config.presets.wezterm;
in
{
  #TODO
  _file = ./wezterm.nix;

  options.presets.wezterm = {
    enable = lib.mkEnableOption "Wezterm with my config";
  };

  config = {
    programs.wezterm = lib.mkIf cfg.enable { enable = true; };
  };
}
