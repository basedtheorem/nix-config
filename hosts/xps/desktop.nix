{ pkgs, ... }:

{
  gnome = {
    enable = true;
    minimal = true;
  };

  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];

  environment.systemPackages = with pkgs; [
    helix # terminal editor
    htop
    intel-gpu-tools
    gtk4
    tela-circle-icon-theme
    polkit

    gnome.gnome-tweaks
    gnomeExtensions.just-perfection # hide panel, overview tweaks
    gnomeExtensions.paperwm # scrolling, tiling wm
    gnomeExtensions.unite # hide title bars
    gnomeExtensions.pano # clipboard manager
    gnomeExtensions.application-volume-mixer

    (graphite-gtk-theme.override {
      wallpapers = true;
      themeVariants = [ "all" ];
    })
  ];
}