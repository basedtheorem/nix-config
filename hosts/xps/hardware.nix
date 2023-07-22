{ config, pkgs, ... }:

{
  _file = ./hardware.nix;

  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;
    keyboard.uhk.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  powerManagement.cpuFreqGovernor = "performance";
  system.stateVersion = "20.09";
}
