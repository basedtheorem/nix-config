{ pkgs, ... }: {
  # TODO: Put dconf2nix output here, backup gnome keybinds and misc conf.

  xdg = {
    configFile."gtk-4.0/gtk.css".text = builtins.readFile ./gtk4.css;
    configFile."gtk-3.0/gtk.css".text = builtins.readFile ./gtk3.css;
    configFile."paperwm/user.css".text = ''
      .paperwm-selection {
        border: 2px solid #7c2c2c !important;
      }
    '';
  };

  home.packages = [ pkgs.flat-remix-gnome ];
  home.sessionVariables.GTK_THEME = "Flat-Remix-GTK-Red-Darkest:dark";

  gtk = {
    enable = true;
    cursorTheme.name = "Bibata-Modern-Classic";
    cursorTheme.package = pkgs.bibata-cursors;

    iconTheme.package = pkgs.flat-remix-icon-theme;
    iconTheme.name = "Flat-Remix-Red-Dark";

    theme.package = pkgs.flat-remix-gtk;
    theme.name = "Flat-Remix-GTK-Red";

    gtk3.extraConfig.gtk-decoration-layout = "menu:";
  };
}
