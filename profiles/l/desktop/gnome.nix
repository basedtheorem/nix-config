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
  home.sessionVariables.GTK_THEME = "Flat-Remix-GTK-Red-Darkest-Solid:dark";

  gtk = {
    enable = true;

    # ~/.config/gtk{3,4}.0/settings.ini
    gtk4.extraConfig = { gtk-application-prefer-dark-theme = 1; };
    gtk3.extraConfig = {
      gtk-decoration-layout = "menu:";
      gtk-application-prefer-dark-theme = 1;
    };

    # ../.gtkrc-2.0
    cursorTheme.name = "Bibata-Modern-Classic";
    cursorTheme.package = pkgs.bibata-cursors;
    iconTheme.package = pkgs.flat-remix-icon-theme;
    iconTheme.name = "Flat-Remix-Red-Dark";
    theme.package = pkgs.flat-remix-gtk;
    theme.name = "Flat-Remix-GTK-Red"; # gsettings get org.gnome.desktop.interface gtk-theme
  };
}
