{ config, lib, ... }:
let
  cfg = config.presets.sioyek;
in
{
  options = {
    presets.sioyek.enable = lib.mkEnableOption "Sioyek (PDF reader)";
  };

  config = lib.mkIf cfg.enable {
    # https://sioyek-documentation.readthedocs.io/en/latest/usage.html
    programs.sioyek = {
      enable = true;

      config = { };

      bindings = { };
    };
  };
}
