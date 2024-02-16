{ pkgs
, config
, ...
}: {
  _file = ./boot.nix;

  fileSystems."/boot".options = [ "umask=0077" ]; # Removes permissions and security warnings.

  boot = {
    initrd.kernelModules = [ "amdgpu" ];

    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      efi.canTouchEfiVariables = true;
      timeout = 0; # Press shift after bootmgr selection for disaster recovery.
      systemd-boot.enable = true;
      systemd-boot.consoleMode = "max";
    };
  };
}
