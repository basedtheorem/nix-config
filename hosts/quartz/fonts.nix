{ pkgs, ... }:
# TODO: move to profiles
{
  fonts = {
    fontconfig = {
      enable = true;
      antialias = true;
      subpixel.lcdfilter = "default";
      cache32Bit = true;
      subpixel.rgba = "rgb";

      hinting = {
        enable = false;
        autohint = false;
        style = "slight";
      };

      defaultFonts = {
        serif = [ "EB Garamond" ];
        sansSerif = [ "Questrial" ];
        monospace = [ "Iosevka Comfy Fixed" ];
        emoji = [ "Twitter Color Emoji" ];
      };
    };

    fontDir.enable = true;

    packages =
      [
        pkgs.iosevka-comfy.comfy-fixed

        (pkgs.nerdfonts.override {
          fonts = [
            "Iosevka"
            "FiraCode"
          ];
        })
        (pkgs.google-fonts.override {
          fonts = [
            "DM Mono"
            "Noto Sans Mono"
            "Cardo"
            "Playfair Display"
            "Readex Pro"
          ];
        })
      ]
      ++ builtins.attrValues {
        inherit (pkgs)
          twitter-color-emoji
          roboto
          victor-mono
          sarasa-gothic
          ;
      };
  };
}
