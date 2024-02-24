{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.terminals.kitty;
in
{
  _file = ./default.nix;

  options.terminals.kitty = {
    enable = lib.mkEnableOption "Kitty with my config";
    grab.enable = lib.mkEnableOption "Kitty Grab";
  };

  config = {
    programs.kitty = lib.mkIf cfg.enable {
      shellIntegration.enableFishIntegration = lib.mkIf config.programs.fish.enable true;

      # > kitty +kitten themes
      # theme = "Glacier";
      # theme = "Wizzy Muted";
      # theme = "Wizzy Bright";
      theme = lib.mkDefault "Black Metal";

      extraConfig = builtins.readFile ./kitty.conf;
    };

    xdg.configFile = lib.mkIf cfg.grab.enable {
      "kitty/kitty_grab".source = inputs.kitty-grab.outPath;
      "kitty/grab.conf".text = ''
        map q quit
        map Ctrl+c confirm
        map Escape quit
      '';
    };
  };
}
