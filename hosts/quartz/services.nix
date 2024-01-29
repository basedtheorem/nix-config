{pkgs, ...}: let
  tuigreet = "${pkgs.greetd.tuigreet}/bin/tuigreet";
in {
  _file = ./services.nix;

  services = {
    fstrim.enable = true;
    thermald.enable = true;
    blueman.enable = true;
    fwupd.enable = true;
    flatpak.enable = true;
    hardware.openrgb.enable = true;
    hardware.openrgb.motherboard = "amd";

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    xserver = {
      enable = true;
      # videoDrivers = ["amdgpu"];

      displayManager = {
        sddm.enable = true;
        sddm.theme = "LentenRose";
        gdm.enable = false;
      };

      layout = "us";
      xkbVariant = "";
      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
      };
    };
  };
  environment.systemPackages = with pkgs; [
    (callPackage ../../packages/lentenrose.nix {})
    libsForQt5.qt5.qtgraphicaleffects
  ];
}
