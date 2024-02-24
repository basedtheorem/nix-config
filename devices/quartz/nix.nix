{
  _file = ./nix.nix;

  config = {
    presets.nix.enable = true;

    nix = {
      extraOptions = ''
        extra-trusted-users = "l"
      '';
    };
  };
}
