{
  self,
  pkgs,
  ...
}:
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
        serif = ["EB Garamond"];
        sansSerif = ["Questrial"];
        monospace = ["Iosevka Comfy"];
        emoji = ["Twitter Color Emoji"];
      };
    };

    fontDir.enable = true;

    packages =
      [
        self.packages."${pkgs.system}".clock-face
        pkgs.iosevka-comfy.comfy

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
          ];
        })
      ]
      ++ builtins.attrValues {
        inherit
          (pkgs)
          twitter-color-emoji
          roboto
          victor-mono
          sarasa-gothic
          ;
      };
  };
}
