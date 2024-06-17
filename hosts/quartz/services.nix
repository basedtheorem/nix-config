{ config, pkgs, ... }:
{
  _file = ./services.nix;

  services = {
    fstrim.enable = true;
    thermald.enable = true;
    blueman.enable = true;
    fwupd.enable = true;
    flatpak.enable = true;
    hardware.openrgb.enable = true;
    hardware.openrgb.motherboard = "amd";
    portmaster.enable = true;
    portmaster.devmode.enable = true;
    portmaster.dataDir = "${config.users.users.l.home}/.local/share/portmaster";

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    libinput = {
      enable = true;
      mouse.accelProfile = "flat";
    };

    displayManager = {
      sddm = {
        enable = true;
        theme = "where_is_my_sddm_theme";
        package = pkgs.kdePackages.sddm;
        extraPackages = [ pkgs.qt6.qt5compat ];
        settings = {
          Wayland.SessionDir = ""; # Prevents auto-selecting wayland
        };
      };
      defaultSession = "gnome-xorg";
    };

    xserver = {
      enable = true;
      xkb.layout = "us";
      displayManager.gdm.enable = false;
    };
  };

  environment.systemPackages = [
    (pkgs.where-is-my-sddm-theme.override {
      themeConfig.General = {
        passwordCharacter = "∗";
        cursorColor = "#FFFFFF";
      };
    })
  ];
}
