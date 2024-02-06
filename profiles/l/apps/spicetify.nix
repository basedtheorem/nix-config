{ pkgs, lib, inputs, ... }:
let spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
in {
  imports = [ inputs.spicetify-nix.homeManagerModule ];

  programs.spicetify = {
    enable = true;
    # theme = spicePkgs.themes.Sleek;
    # colorScheme = "VantaBlack";
    theme = spicePkgs.themes.Ziro;
    colorScheme = "Red-Dark";

    enabledExtensions = with spicePkgs.extensions; [
      fullAppDisplay
      shuffle # shuffle+ (special characters are sanitized out of ext names)
      hidePodcasts
      popupLyrics
      autoSkipVideo

      powerBar
      seekSong
      history
      hidePodcasts
      adblock
    ];
  };
}
