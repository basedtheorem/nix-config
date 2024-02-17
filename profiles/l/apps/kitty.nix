{inputs, ...}: {
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;

    # > kitty +kitten themes
    # theme = "Glacier";
    theme = "Black Metal";
    # theme = "Wizzy Muted";
    # theme = "Wizzy Bright";

    extraConfig = builtins.readFile ../sources/kitty.conf;
  };

  xdg.configFile."kitty/kitty_grab".source = inputs.kitty-grab.outPath;
  xdg.configFile."kitty/grab.conf".text = ''
    map q quit
    map Ctrl+c confirm
    map Escape quit
  '';
}
