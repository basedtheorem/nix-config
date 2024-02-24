{ config, lib, ... }:
let
  cfg = config.presets.base;
in
{
  _file = ./default.nix;

  imports = [
    ./micro # Replaces nano
  ];

  options.presets.base = {
    enable = lib.mkEnableOption "Base configuration (common defaults)";
  };

  config = lib.mkIf cfg.enable {
    home-manager.enable = true;
    news.display = lib.mkForce "silent";

    nixpkgs.config = {
      allowUnfree = true;
    };
  };
}
