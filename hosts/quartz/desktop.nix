{
  lib,
  pkgs,
  ...
}: {
  _file = ./desktop.nix;

  gnome = {
    enable = true;
    minimal = true;
  };

  programs = {
    fish.enable = true;
    kdeconnect.enable = false;
  };

  environment.shells = [pkgs.fish pkgs.nushell];

  environment.systemPackages = with pkgs;
    [
      micro
      git
      firefox
      openrgb-with-all-plugins
      nvtop-amd
      woeusb-ng # create bootable USB disks from windows ISO images
      ntfs3g # needed for mkntfs command (woeusb)

      # Gnome
      gnome.gnome-tweaks
      (graphite-gtk-theme.override {
        themeVariants = ["all"];
      })
    ]
    ++ (with pkgs.gnomeExtensions; [
      paperwm # scrolling, tiling wm
      # blur-my-shell
      # just-perfection # overview tweaks + hide panel
      smile-complementary-extension
      unite # hide title bars
      pano # clipboard manager
      search-light
      vertical-workspaces
    ]);
}
