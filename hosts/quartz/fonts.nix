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
        serif = ["EB Garamond" "Symbols Nerd Font Mono"];
        sansSerif = ["Questrial" "Symbols Nerd Font Mono"];
        monospace = ["Iosevka Comfy" "Symbols Nerd Font Mono"];
        emoji = ["Twitter Color Emoji"];
      };
    };

    fontDir.enable = true;

    packages = with pkgs; [
      (nerdfonts.override {
        fonts = [
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
      openmoji-black
      material-design-icons
      roboto
      iosevka
      iosevka-comfy.comfy
      office-code-pro
      victor-mono
      noto-fonts
      mplus-outline-fonts.githubRelease
    ];
  };
}
