{
  lib,
  pkgs,
  inputs,
  ...
}: {
  _file = ./services.nix;

  services = {
    fstrim.enable = true;
    thermald.enable = true;
    blueman.enable = true;
    fwupd.enable = true;
    opensnitch.enable = true;
    flatpak.enable = true;
    hardware.openrgb.enable = true;
    hardware.openrgb.motherboard = "amd";
    mullvad-vpn.enable = true;
    mullvad-vpn.package = pkgs.mullvad-vpn;
    resolved.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    xserver = {
      enable = true;
      videoDrivers = ["amdgpu"];

      displayManager = {
        sddm = {
          enable = true;
          theme = "where_is_my_sddm_theme";
          settings = {
            Wayland.SessionDir = ""; # Prevents auto-selecting wayland
          };
        };
        defaultSession = "gnome";
        gdm.enable = false;
      };

      xkb.layout = "us";
      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
      };
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
