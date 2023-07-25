{ pkgs, ... }:

{
  gtk.enable = true;

  gtk.cursorTheme = {
    package = pkgs.graphite-cursors;
    name = "Graphite dark Cursors";
    size = 16;
  };
}