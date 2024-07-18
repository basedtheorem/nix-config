{ self, pkgs, ... }:
{
  _file = ./desktop.nix;

  config = {
    presets.gnome = {
      enable = true;
      minimal = true;
    };

    environment.systemPackages = builtins.attrValues {
      # Gnome.
      inherit (pkgs) gnome-tweaks;
      inherit (pkgs.gnomeExtensions)
        paperwm # scrolling, tiling wm
        blur-my-shell
        just-perfection # remove annoying notifications!!!! f**k
        smile-complementary-extension # allow paste on option select
        impatience # increase animation speed
        zen # mouse follows focus
        pano # clipboard manager
        ;
    };
  };
}
