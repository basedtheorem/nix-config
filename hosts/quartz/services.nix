{pkgs, ...}: {
  _file = ./services.nix;

  services = {
    fstrim.enable = true;
    thermald.enable = true;
    blueman.enable = true;
    fwupd.enable = true;

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

      layout = "us";
      xkbVariant = "";
      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
      };
    };
  };
}
