{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:
let
  cfg = config.presets.discord;
in
{
  options = {
    presets.discord.enable = lib.mkEnableOption "Discord";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.betterdiscordctl # run `betterdiscordctl install` then restart dc
      (pkgs.discord.override { withOpenASAR = true; })
    ];

    xdg.configFile = {
      "BetterDiscord/themes/chillax.theme.css".source =
        inputs.chillax-discord-theme + "/chillax.theme.css";
    };
  };
}
