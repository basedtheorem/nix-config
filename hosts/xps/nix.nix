{
  _file = ./nix.nix;

  nix = {
    settings = {
      extra-trusted-users = [ "l" ];
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 1w";
    };
  };
}
