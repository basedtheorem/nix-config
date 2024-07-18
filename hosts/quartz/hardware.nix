{
  config,
  lib,
  pkgs,
  ...
}:
{
  _file = ./hardware.nix;

  hardware = {
    pulseaudio.enable = false;
    keyboard.uhk.enable = true;
    graphics = {
      enable = true;
      extraPackages = [ pkgs.amdvlk ];
    };
  };

  environment.systemPackages = lib.mkIf config.hardware.keyboard.uhk.enable [ pkgs.uhk-agent ];

  security.rtkit.enable = true;

  system.stateVersion = "23.11";
}
