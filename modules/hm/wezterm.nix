{ lib, config, ... }:
let
  cfg = config.terminals.wezterm;
in
{
  #TODO
  _file = ./wezterm.nix;

  options.terminals.wezterm = {
    enable = lib.mkEnableOption "Wezterm with my config";
  };

  config = {
    programs.wezterm = lib.mkIf cfg.enable { enable = true; };
  };
}
