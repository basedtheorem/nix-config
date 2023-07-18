{ config, pkgs, ... }:

{
  _file = ./hardware.nix;

  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;
    keyboard.uhk.enable = true;
    cpu.intel.updateMicrocode = true;

    bumblebee.enable = true;
    bumblebee.pmMethod = "none";
    # nvidia = {
    #   modesetting.enable = true;
    #   prime = {
    #     sync.enable = true;
    #     intelBusId = "0@0:2:0";
    #     nvidiaBusId = "1@0:0:0";
    #   };
    # };

    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver
        intel-vaapi-driver
        libvdpau-va-gl
      ];
    };

  };

  environment.variables = {
    VDPAU_DRIVER = "va_gl";
  };

  powerManagement.cpuFreqGovernor = "performance";
  system.stateVersion = "20.09";
}
