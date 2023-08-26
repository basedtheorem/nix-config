{ pkgs, ... }:
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
        serif = [ "Readex Pro" "Symbols Nerd Font Mono" ];
        sansSerif = [ "Readex Pro" "Symbols Nerd Font Mono" ];
        monospace = [ "M PLUS 1 Code" "Symbols Nerd Font Mono" ];
        emoji = [ "JoyPixels" "OpenMoji Black" "EmojiOne Color" "Twitter Color Emoji" ];
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
      joypixels
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
