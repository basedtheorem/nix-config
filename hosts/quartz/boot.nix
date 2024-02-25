{ pkgs, config, ... }:
{
  _file = ./boot.nix;

  fileSystems."/boot".options = [ "umask=0077" ]; # Removes permissions and security warnings.

  boot = {
    initrd.kernelModules = [ "amdgpu" ];

    kernelParams = [
      # head /sys/class/drm/*/status
      # Example:
      # "video=DP-1:2560x1440@74.97"
      # "video=HDMI-A-1:1920x1080@59.94"
    ];

    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      efi.canTouchEfiVariables = true;
      timeout = 0; # Press shift after bootmgr selection for disaster recovery.
      systemd-boot.enable = true;
      systemd-boot.consoleMode = "max";
    };
  };
}
