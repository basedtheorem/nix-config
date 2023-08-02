{ pkgs, ... }:

{
  home.pointerCursor = {
    package = pkgs.graphite-cursors;
    name = "Graphite-Cursors";
    size = 16;
    x11.enable = true;
  };
}