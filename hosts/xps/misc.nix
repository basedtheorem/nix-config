{ pkgs, ... }:

{
  _file = ./misc.nix;
  networking.hostName = "xps";
  networking.networkmanager.enable = true;

  console.font = "${pkgs.terminus_font}/share/consolefonts/ter-v32n.psf.gz";
  console.earlySetup = true;
  
  time.timeZone = "Pacific/Auckland";
  i18n.defaultLocale = "en_US.UTF-8";
}
