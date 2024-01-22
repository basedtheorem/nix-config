{...} @ all: {
  _file = ./default.nix;

  perSystem = {
    system,
    pkgs,
    lib,
    inputs',
    ...
  }: {
    packages = {
      v-shell = pkgs.callPackage ./v-shell.nix {};
      swhkd = pkgs.callPackage ./swhkd.nix {};
      lentenrose = pkgs.callPackage ./lentenrose.nix {};
    };
  };
}
