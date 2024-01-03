{inputs, ...}: {
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
      micro-autofmt = pkgs.callPackage ./micro-autofmt.nix {source = inputs.micro-autofmt;};
    };
  };
}
