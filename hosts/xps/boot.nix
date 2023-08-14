{ pkgs, config, ... }:

{
  _file = ./boot.nix;

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot/efi";
      grub.efiSupport = true;
      grub.device = "nodev";
      grub.configurationLimit = 10;
    };

    initrd.kernelModules = [ "i915" ];

    kernelParams = [ "mem_sleep_default=deep" "nvidia-drm.modeset=1" "acpi_rev_override" ];

    # extraModprobeConfig = ''
    #   options bbswitch load_state=-1 unload_state=1 nvidia-drm
    # '';

    #kernelPackages = pkgs.linuxPackages_latest;
  };
}
