{
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
      videoDrivers = ["amdgpu"];

      displayManager = {
        sddm = {
          enable = true;
          theme = "where_is_my_sddm_theme";
        };
        defaultSession = "gnome";
        gdm.enable = false;
      };

      layout = "us";
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
