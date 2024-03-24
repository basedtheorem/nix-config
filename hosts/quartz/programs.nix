{ pkgs, ... }:

{
  programs = {
    fish.enable = true;
    kdeconnect.enable = false;
    nix-ld = {
      enable = true;

      libraries = builtins.attrValues {
        # inherit (pkgs)
        # Add missing dynamic libs here
      };
    };
  };
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      micro
      git
      firefox
      openrgb-with-all-plugins
      woeusb-ng # create bootable USB disks from windows ISO images
      ntfs3g # needed for mkntfs command (woeusb)
      ;
    inherit (pkgs.nvtopPackages) amd;
  };
  environment.shells = [
    pkgs.fish
    pkgs.nushell
  ];
}
