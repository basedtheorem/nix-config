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
    programs.fish.interactiveShellInit = lib.mkIf fishEnabled ''
      zoxide init fish | source
    '';
  };
}
