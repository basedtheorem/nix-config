{ pkgs, ... }:

{
  home.pointerCursor = {
    package = pkgs.graphite-cursors;
    name = "Graphite-Cursors";
    size = 32;
    x11.enable = true;
  };
}