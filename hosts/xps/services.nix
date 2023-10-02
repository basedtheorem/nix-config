{ pkgs, ... }:

{
  _file = ./services.nix;

  services = {
    fstrim.enable = true;
    thermald.enable = true;
    blueman.enable = true;
    fwupd.enable = true;
    gnome.sushi.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = false;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    xserver = {
      layout = "us";
      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
      };
    };
  };
}
