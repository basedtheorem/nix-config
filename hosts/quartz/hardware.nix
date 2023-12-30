{
  config,
  pkgs,
  ...
}: {
  _file = ./hardware.nix;

  hardware = {
    pulseaudio.enable = false;
    keyboard.uhk.enable = true;
  };

  # Enable sound with pipewire.
  sound.enable = true;

  security.rtkit.enable = true;

  system.stateVersion = "23.11";
}
