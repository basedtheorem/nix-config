{
  _file = ./nix.nix;

  # TODO should extend rather than overwrite
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    extra-trusted-users = "l"
  '';
}
