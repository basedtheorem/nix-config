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
    opengl = {
      enable = true;
      extraPackages = with pkgs; [ amdvlk ];
    };
  };

  environment.systemPackages = lib.mkIf config.hardware.keyboard.uhk.enable [ pkgs.uhk-agent ];

  security.rtkit.enable = true;

  system.stateVersion = "23.11";
}
