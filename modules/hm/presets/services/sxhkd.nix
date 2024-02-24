{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.sxhkd;
in
{
  _file = ./sxhkd.nix;

  options = {
    presets.sxhkd.enable = lib.mkEnableOption "sxhkd (run commands with keybindings)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.socat # pass cmds to mpv socket
      pkgs.playerctl
    ];

    services.playerctld.enable = true;

    services.sxhkd = {
      enable = true;

      keybindings = {
        "super + shift + {F7,F9}" = "echo 'add speed {-,+}0.1' | socat - /tmp/mpvsocket";
        "super + shift + ctrl + alt + {KP_Left,KP_Right}" = "playerctl position 5{-,+}";
      };
    };
  };
}
