{ config, lib, ... }:
let
  cfg = config.profiles.base;
in
# System agnostic configuration (hopefully)
{
  _file = ./base.nix;


  options.profiles.base = {
    enable = lib.mkEnableOption "Base configuration";
  };

  config = lib.mkIf cfg.enable {
    presets.micro.enable = true; # Replaces nano
    presets.fzf.enable = true;

    programs = {
      home-manager.enable = true;
    };

    news.display = lib.mkForce "silent";

    nixpkgs.config = {
      allowUnfree = true;
    };
  };
}
