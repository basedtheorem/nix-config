{ pkgs, ... }: {
  home.packages = [
    pkgs.socat # pass cmds to mpv socket
  ];
  services.sxhkd = {
    enable = true;

    keybindings = {
      "super + shift + {F7,F9}" = "echo 'add speed {-,+}0.1' | socat - /tmp/mpvsocket";
      "super + shift + ctrl + alt + {KP_Left,KP_Right}" = "playerctl position 5{-,+}";
    };
  };
}
