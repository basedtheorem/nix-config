{ pkgs, ... }:

{
  home.pointerCursor = {
    package = pkgs.graphite-cursors;
    name = "Graphite-Cursors";
    size = 12;
    x11.enable = true;
  };
}
