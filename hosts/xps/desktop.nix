{ lib, pkgs, ... }:

{
  _file = ./desktop.nix;

  gnome = {
    enable = true;
    minimal = true;
  };

  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];

  environment.systemPackages = with pkgs; [
    helix # terminal editor
    btop
    intel-gpu-tools
    gtk4
    tela-circle-icon-theme
    polkit
    

    gnome.gnome-tweaks
    gnomeExtensions.rounded-window-corners
    gnomeExtensions.just-perfection # hide panel, overview tweaks
    gnomeExtensions.paperwm # scrolling, tiling wm
    gnomeExtensions.unite # hide title bars
    gnomeExtensions.pano # clipboard manager
    (callPackage ../../packages/v-shell.nix {}) # up-to-date version

    (graphite-gtk-theme.override {
      wallpapers = true;
      themeVariants = [ "all" ];
    })
  ];
}
