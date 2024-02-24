{
  config = {
    presets = {
      main.enable = true;
      base.enable = true;
      dev.enable = true;
    };

    terminals = {
      kitty.enable = true;
      wezterm.enable = false;
    };

    home = {
      username = "l";
      homeDirectory = "/home/l";
      stateVersion = "22.11";
    };

    programs = {

      emacs.enable = true;
      micro.enable = true;

      eza.enable = true; # ls alt.

      yazi.enable = true;

      git = {
        userEmail = "lrns@proton.me";
        userName = "1rns";
      };
    };
  };
}
