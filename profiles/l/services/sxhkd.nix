{ pkgs, ... }:

{
  services.sxhkd = {
    enable = true;

    keybindings = {
      "super + shift + {F7,F9}" = "echo 'add speed {-,+}0.1' | socat - $\{XDG_CONFIG_HOME\}/mpv/socket";
      "super + shift + p" = "flameshot gui";
    };
  };

  systemd.user.services.sxhkd = {
    Unit = {
      Description = "xbanish hides the mouse pointer";
      PartOf = [ "graphical-session.target" ];
    };
    
    Service = {
      ExecStart = ''
        ${pkgs.sxhkd}/bin/sxhkd
      '';
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
