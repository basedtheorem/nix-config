{ config, lib, ... }:
let
  cfgp = config.programs;
  yaziEnabled = config.presets.yazi.enable;
in
{
  _file = ./yazi.nix;

  options = {
    presets.yazi.enable = lib.mkEnableOption "Yazi";
  };

  config = lib.mkIf yaziEnabled {
    programs.fish = {
      shellAbbrs = lib.mkIf cfgp.fish.enable { fm = "ya"; };
    };

    programs.yazi = {
      enable = true;

      enableFishIntegration = lib.mkIf cfgp.fish.enable true;

      theme = {
        filetype = {
          rules = [
            {
              fg = "#7AD9E5";
              mime = "image/*";
            }
            {
              fg = "#F3D398";
              mime = "video/*";
            }
            {
              fg = "#F3D398";
              mime = "audio/*";
            }
            {
              fg = "#CD9EFC";
              mime = "application/x-bzip";
            }
          ];
        };
      };

      keymap = {
        # input.keymap = [
        #   {
        #     exec = "close";
        #     on = [ "<C-q>" ];
        #   }
        #   {
        #     exec = "close --submit";
        #     on = [ "<Enter>" ];
        #   }
        #   {
        #     exec = "escape";
        #     on = [ "<Esc>" ];
        #   }
        # ];
      };

      settings.manager = {
        sort_by = "modified";
        sort_dir_first = true;
      };
    };
  };
}
