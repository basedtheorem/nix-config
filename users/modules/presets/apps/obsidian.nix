{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.presets.obsidian;
in
{
  options = {
    presets.obsidian.enable = lib.mkEnableOption "Obsidian";
  };

  config = lib.mkIf cfg.enable { home.packages = [ pkgs.obsidian ]; };
}
