{ config, lib, ... }:
let
  lazygitEnabled = config.programs.lazygit.enable;
in
{
  _file = ./lazygit.nix;
  config = {
    home.shellAliases = lib.mkIf lazygitEnabled { lg = "lazygit"; };

    programs.lazygit = {
      enable = true;
    };
  };
}
