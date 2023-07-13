{ pkgs, ... }:

{
  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;
    keyboard.uhk.enable = true;
    cpu.intel.updateMicrocode = config.hardware.enableRedistributableFirmware;

    nvidia = {
      modesetting.enable = true;
      prime = {
        sync.enable = true;
        intelBusId = "0@0:2:0";
        nvidiaBusId = "1@0:0:0";
      };
    };

    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
  };


  powerManagement.cpuFreqGovernor = "performance";
  system.stateVersion = "20.09";
}