{
  pkgs,
  config,
  ...
}: {
  _file = ./boot.nix;

 boot = {
   loader = {
     efi.canTouchEfiVariables = true;
     grub = {
       enable = true;
       device = "nodev";
       efiSupport = true;
       theme = pkgs.sleek-grub-theme.override {withBanner = "Laurens' PC";};
       configurationLimit = 10;
     };
   };
 };
}
