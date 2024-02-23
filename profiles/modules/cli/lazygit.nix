{ config, lib, ... }:
let
  lazygitEnabled = config.programs.lazygit.enable;
in
{
  programs.lazygit = {
    enable = true;
  };
  home.shellAliases = lib.mkIf lazygitEnabled { lg = "lazygit"; };
}
