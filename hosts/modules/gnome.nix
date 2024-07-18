{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.presets.gnome;
in
{
  _file = ./gnome.nix;

  # TODO:
  # https://determinate.systems/posts/declarative-gnome-configuration-with-nixos

  options.presets.gnome = {
    enable = lib.mkEnableOption "Gnome Desktop Environment";

    minimal = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether to remove most of the pre-installed applications
        that come with the Gnome Desktop Environment.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      displayManager.gdm.enable = lib.mkDefault true;
      displayManager.gdm.wayland = false;
      desktopManager.gnome.enable = true;
    };

    environment.systemPackages = builtins.attrValues {
      inherit (pkgs)
        xdg-desktop-portal # Fixes "No such interface..."
        xdg-desktop-portal-gnome
        xdg-desktop-portal-gtk
        nautilus-open-any-terminal

        # Gnome
        gnome-firmware
        sushi
        file-roller
        ;

      inherit (pkgs.gnome) gvfs gnome-session;
    };

    environment.gnome = lib.mkIf cfg.minimal {
      excludePackages = builtins.attrValues {
        inherit (pkgs)
          gnome-photos
          gnome-tour
          gedit
          cheese # webcam tool
          gnome-terminal
          simple-scan
          epiphany # web browser
          geary # email reader
          totem # video player
          evince # document viewer
          ;

        inherit (pkgs.gnome)
          gnome-music
          tali # poker game
          iagno # go game
          hitori # sudoku game
          atomix # puzzle game
          ;
      };
    };
  };
}
