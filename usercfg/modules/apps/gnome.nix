{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.presets.gnome;
in
{
  # TODO: Put dconf2nix output here, backup gnome keybinds and misc conf.
  options = {
    presets.gnome.enable = lib.mkEnableOption "Gnome";
  };

  config = lib.mkIf cfg.enable {
    xdg = {
      configFile."gtk-4.0/gtk.css".text = builtins.readFile ./gtk4.css;
      configFile."gtk-3.0/gtk.css".text = builtins.readFile ./gtk3.css;
      configFile."paperwm/user.css".text = ''
        .paperwm-selection {
          border: 20px solid #7c2c2c !important;
          /* border-color: 20px solid #7c2c2c !important; */
          opacity: 100;
        }
      '';
    };

    home.packages = [ pkgs.flat-remix-gnome ];
    home.sessionVariables.GTK_THEME = "Orchis-Red-Dark-Compact:dark";

    gtk = {
      enable = true;

      # ~/.config/gtk{3,4}.0/settings.ini
      gtk4.extraConfig = {
        gtk-application-prefer-dark-theme = 1;
      };
      gtk3.extraConfig = {
        gtk-decoration-layout = "menu:";
        gtk-application-prefer-dark-theme = 1;
      };

      # ../.gtkrc-2.0
      cursorTheme.name = "volantes_cursors";
      cursorTheme.size = 32;
      cursorTheme.package = pkgs.volantes-cursors;
      iconTheme.package = pkgs.flat-remix-icon-theme;
      iconTheme.name = "Flat-Remix-Red-Dark";
      theme.package = pkgs.orchis-theme.override {
        tweaks = [
          "black"
          "solid"
          "compact"
          "submenu"
        ];
      };
      theme.name = "Orchis-Red-Dark-Compact"; # gsettings get org.gnome.desktop.interface gtk-theme
    };
  };
}
