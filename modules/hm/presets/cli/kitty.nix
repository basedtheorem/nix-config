{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.presets.kitty;
in
{
  _file = ./kitty.nix;

  options.presets.kitty = {
    enable = lib.mkEnableOption "Kitty terminal";
    grab.enable = lib.mkEnableOption "Kitty Grab";
  };

  config = lib.mkIf cfg.enable {
    presets.kitty.grab.enable = lib.mkDefault true;

    programs.kitty = {
      enable = true;
      shellIntegration.enableFishIntegration = lib.mkIf config.programs.fish.enable true;

      # $ kitty +kitten themes
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
