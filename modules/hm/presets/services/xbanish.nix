{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.presets.xbanish;
in
{
  _file = ./xbanish.nix;

  options = {
    presets.xbanish.enable = lib.mkEnableOption "xbanish (hide cursor when typing)";
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.xbanish = {
      Unit = {
        Description = "xbanish hides the mouse pointer";
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = ''
          ${pkgs.xbanish}/bin/xbanish
        '';
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
