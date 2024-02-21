{inputs, ...}: {
  _file = ./default.nix;

  perSystem = {
    system,
    pkgs,
    ...
  }: {
    packages = {
      v-shell = pkgs.callPackage ./v-shell.nix {};
      swhkd = pkgs.callPackage ./swhkd.nix {};
      lentenrose = pkgs.callPackage ./lentenrose.nix {};
      clock-face = (import inputs.master {
          inherit system;
          config.allowUnfree = true;
      }).callPackage ./clock-face.nix {};
    };
  };
}
