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
      displayManager.gdm.enable = lib.mkDefault true;
      displayManager.gdm.wayland = false;
      desktopManager.gnome.enable = true;
    };

    environment.systemPackages = with pkgs; [
      gnome-firmware
      gnome.gvfs
      gnome.sushi
      gnome.file-roller
      gnome.gnome-session
      nautilus-open-any-terminal
    ];

    environment.gnome = lib.mkIf cfg.minimal {
      excludePackages = (with pkgs; [ gnome-photos gnome-tour gedit ])
        ++ (with pkgs.gnome; [
        cheese # webcam tool
        gnome-music
        gnome-terminal
        simple-scan
        epiphany # web browser
        geary # email reader
        geary
        evince # document viewer
        totem # video player
        tali # poker game
        iagno # go game
        hitori # sudoku game
        atomix # puzzle game
      ]);
    };
  };
}
