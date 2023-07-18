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

    kernelParams = [
      "acpi_rev_override"#"mem_sleep_default=deep" "intel_iommu=igfx_off" "nvidia-drm.modeset=1"
    ];

    # extraModprobeConfig = ''
    #   options bbswitch load_state=-1 unload_state=1 nvidia-drm
    # '';

    blacklistedKernelModules = [
      "nouveau"
      "bbswitch"
    ];

    kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];
  };
}
