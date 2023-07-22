{
  _file = ./services.nix;

  services = {
    mullvad-vpn.enable = false;
    fstrim.enable = true;
    thermald.enable = true;
    blueman.enable = true;

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
