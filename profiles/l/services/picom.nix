{
  services.picom = {enable = false;};

  xdg.configFile."picom/picom.conf".source = ../sources/picom.conf;
}
