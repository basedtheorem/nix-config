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
      ;
    inherit (pkgs.nvtopPackages) amd;
  };
  environment.shells = [
    pkgs.fish
    pkgs.nushell
  ];
}
