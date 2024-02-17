{
  pkgs,
  inputs,
  ...
}: {
  config = {
    home.packages = with pkgs; [
      betterdiscordctl # run `betterdiscordctl install` then restart dc
      (discord.override {withOpenASAR = true;})
    ];

    xdg.configFile = {
      "BetterDiscord/themes/chillax.theme.css".source =
        inputs.chillax-discord-theme + "/chillax.theme.css";
    };
  };
}
