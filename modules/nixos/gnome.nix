{ config, lib, pkgs, ... }:

let cfg = config.gnome;

in {
  _file = ./gnome.nix;

  # TODO:
  # https://determinate.systems/posts/declarative-gnome-configuration-with-nixos

  options.gnome = {
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
      enable = true;
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false;
      desktopManager.gnome.enable = true;
    };

    environment.systemPackages = with pkgs; [
      gnome-firmware
      gnome.nautilus
      gnome.gvfs
      gnome.sushi
      gnome.file-roller
      nautilus-open-any-terminal
    ];

    environment.gnome = lib.mkIf cfg.minimal {
    excludePackages =
      (with pkgs; [
        gnome-photos
        gnome-tour
      ])
      ++ (with pkgs.gnome; [
        cheese # webcam tool
        gnome-music
        gnome-terminal
        simple-scan
        #eog # image viewer
        gedit # text editor
        epiphany # web browser
        geary # email reader
        gnome-software
        geary
        evince # document viewer
        gnome-characters
        totem # video player
        tali # poker game
        iagno # go game
        hitori # sudoku game
        atomix # puzzle game
      ]);
    };
  };
}
