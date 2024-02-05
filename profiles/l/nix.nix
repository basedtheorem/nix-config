{self, ...}: {
  systemd.user.sessionVariables = {
    NIXPKGS_ALLOW_UNFREE = 1;
  };

  nixpkgs.overlays = [ self.overlays.micro ];
}
