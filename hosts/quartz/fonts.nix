{pkgs, ...}:
# TODO: move to profiles
{
  fonts = {
    fontconfig = {
      enable = true;
      antialias = true;
      subpixel.lcdfilter = "default";
      subpixel.rgba = "rgb";

      hinting = {
        enable = true;
        autohint = true;
        style = "slight";
      };

      defaultFonts = {
        serif = ["EB Garamond"];
        sansSerif = ["Questrial"];
        monospace = ["Iosevka Comfy"];
        emoji = ["Twitter Color Emoji"];
      };
    };

    fontDir.enable = true;

    packages = with pkgs; [
      (nerdfonts.override {
        fonts = [
          "Iosevka"
          "FiraCode"
          "DroidSansMono"
          "Gohu"
          "NerdFontsSymbolsOnly"
        ];
      })

      mononoki
      ankacoder
      google-fonts
      twitter-color-emoji
      emojione
      material-design-icons
      roboto
      iosevka-comfy.comfy
      office-code-pro
      victor-mono
      noto-fonts
      mplus-outline-fonts.githubRelease
    ];
  };
}
