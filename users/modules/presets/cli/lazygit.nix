{ config, lib, ... }:
let
  cfg = config.presets.lazygit;
in
{
  _file = ./lazygit.nix;

  options = {
    presets.lazygit.enable = lib.mkEnableOption "Lazygit";
  };

  config = {
    home.shellAliases = lib.mkIf cfg.enable { lg = "lazygit"; };

    programs.lazygit = {
      enable = true;
    };
  };
}
