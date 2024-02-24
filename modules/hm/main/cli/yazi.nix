{ config, lib, ... }:
let
  cfgp = config.programs;
in
{
  _file = ./yazi.nix;

  programs.fish = {
    shellAbbrs = lib.mkIf cfgp.fish.enable { fm = "ya"; };
  };

  programs.yazi = lib.mkIf cfgp.yazi.enable {
    enableFishIntegration = lib.mkIf cfgp.fish.enable true;
    keymap = {
      input.keymap = [
        {
          exec = "close";
          on = [ "<C-q>" ];
        }
        {
          exec = "close --submit";
          on = [ "<Enter>" ];
        }
        {
          exec = "escape";
          on = [ "<Esc>" ];
        }
      ];
    };

    settings.manager = {
      sort_by = "modified";
      sort_dir_first = true;
    };
  };
}
