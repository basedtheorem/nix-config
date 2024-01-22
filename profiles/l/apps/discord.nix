{
  pkgs,
  inputs,
  ...
}: let
  darkmatter-theme = pkgs.fetchFromGitHub {
    owner = "DiscordStyles";
    repo = "DarkMatter";
    rev = "3fe701e79d89954980c626685943180579337506";
    sha256 = "sha256-4crLhVnpDN2theH9ypTpSM/5XWUc8fcNlbpsfluGdn0=";
  };
in {
  config = {
    home.packages = with pkgs; [
      betterdiscordctl # run `betterdiscordctl install` then restart dc
      (discord-ptb.override {
        withOpenASAR = true;
      })
    ];

    xdg.configFile = {
      "BetterDiscord/themes/DarkMatter.theme.css".source = inputs.darkmatter-discord-theme + "/DarkMatter.theme.css";
    };
  };
}
