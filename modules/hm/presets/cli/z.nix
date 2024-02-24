{ config, lib, ... }:
let
  zEnabled = config.presets.z.enable;
  fishEnabled = config.programs.fish.enable;
in
{
  options = {
    presets.z.enable = lib.mkEnableOption "Zoxide (a cd alternative)";
  };

  config = lib.mkIf zEnabled {
    programs.zoxide = {
      enable = true;
      enableFishIntegration = lib.mkIf fishEnabled true;
    };

    programs.fish = lib.mkIf fishEnabled {
      interactiveShellInit = ''
        zoxide init fish | source
      '';
      shellAbbrs.cd = "z";
    };
  };
}
