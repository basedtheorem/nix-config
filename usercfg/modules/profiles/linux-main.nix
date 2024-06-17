{ config, lib, ... }:
let
  cfg = config.profiles.linux-main;
in
{
  _file = ./default.nix;

  options.profiles.linux-main = {
    enable = lib.mkEnableOption "Enable current Linux desktop setup.";
  };

  config.presets = lib.mkIf cfg.enable {
    # Apps
    discord.enable = true;
    emacs.enable = true;
    flatpak.enable = true;
    gnome.enable = true;
    obs.enable = true;
    obsidian.enable = true;
    sioyek.enable = true;
    spicetify.enable = true;
    vivaldi.enable = true;
    vscodium.enable = true;

    # CLI
    eza.enable = true;
    fish.enable = true;
    fzf.enable = true;
    kitty.enable = true;
    lazygit.enable = true;
    mpv.enable = true;
    yazi.enable = true;
    z.enable = true;

    # Services
    espanso.enable = true;
    flameshot.enable = true;
    sxhkd.enable = true;
    syncthing.enable = true;
    ulauncher.enable = true;
    xbanish.enable = true;
  };
}
