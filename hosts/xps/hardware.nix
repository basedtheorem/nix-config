{ config, pkgs, ... }:

{
  _file = ./hardware.nix;

  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;
    keyboard.uhk.enable = true;
    cpu.intel.updateMicrocode = true;

    nvidia = {
      modesetting.enable = true;
      nvidiaSettings = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
      prime = {
        offload.enable = true;
        offload.enableOffloadCmd = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
    };

    opengl = {
      enable = true;
      #driSupport = true;
      #driSupport32Bit = true;
      extraPackages = with pkgs; [
        intel-media-driver
        vaapiIntel
        vaapiVdpau
        nvidia-vaapi-driver
        libvdpau-va-gl
      ];
    };

  };

  environment.variables = {
    LIBVA_DRIVER_NAME = "iHD";
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  powerManagement.cpuFreqGovernor = "performance";
  system.stateVersion = "20.09";
}
