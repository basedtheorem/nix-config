{
  self,
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.presets.main;
in
rec {
  _file = ./default.nix;

  options.presets.main = {
    enable = lib.mkEnableOption "My default Linux setup";
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge self.lib.readNixExpsFrom {
      path = ./.;
      excludes = _file;
    }
  );
}
