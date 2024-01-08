{
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;

    # > kitty +kitten themes
    # theme = "Glacier";
    # theme = "Wizzy Muted";
    theme = "Wizzy Bright";

    extraConfig = builtins.readFile ../sources/kitty.conf;
  };
}
