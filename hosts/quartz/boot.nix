{
  pkgs,
  config,
  ...
}: {
  _file = ./boot.nix;

 boot = {
   loader = {
     efi.canTouchEfiVariables = true;

     systemd-boot.enable = true;
     timeout = 0;

     # grub = {
     #   enable = true;
     #   devices = [ "nodev" ];
     #   useOSProber = true;
     #   efiSupport = true;
     #   theme = pkgs.sleek-grub-theme.override {withBanner = "Laurens' PC"; withStyle = "bigSur"};
     #   # override `withStyle` to specify one of "dark" / "orange" / "bigSur"
     #   configurationLimit = 5;
     # };
   };
 };
}
