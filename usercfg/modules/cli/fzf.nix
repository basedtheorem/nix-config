{ config, lib, ... }:

let
  cfg = config.presets.fzf;
  zoxideEnabled = config.programs.zoxide.enable;
in
{
  _file = ./fzf.nix;

  options.presets.fzf = {
    enable = lib.mkEnableOption "Themed fzf";
  };

  config = lib.mkIf cfg.enable {
    programs.fzf = {
      enable = true;

      changeDirWidgetCommand = lib.mkIf zoxideEnabled "z";
      colors = {
        "fg" = "-1";
        "fg+" = "#fe6060";
        "bg" = "-1";
        "bg+" = "#000000";
        "hl" = "#b05f5f";
        "hl+" = "#fe6060";
        "info" = "#b15c5c";
        "marker" = "#ffae00";
        "prompt" = "#ff6b6b";
        "border" = "#000000";
        "spinner" = "#ff5e5e";
        "pointer" = "#ff5e5e";
        "header" = "#ff914d";
        "label" = "#aeaeae";
        "query" = "#d9d9d9";
      };
      defaultOptions = [
        "--preview-window='border-sharp'"
        "--margin='1'"
        "--prompt='Search > '"
        "--marker='> '"
        "--pointer='◆'"
        "--separator='─'"
        "--scrollbar='.'"
        "--layout='reverse'"
        "--info='right'"
      ];
    };
  };
}
