{ pkgs, ... }: {

  programs.rofi = {
    enable = false;

    plugins = [
    pkgs.rofi-calc
    pkgs.rofi-emoji
    ];
  };
  
  xdg.configFile."rofi".source = ../sources/rofi;
  xdg.configFile."rofi".recursive = true;
}