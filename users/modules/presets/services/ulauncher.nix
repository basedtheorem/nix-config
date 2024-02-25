{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.presets.ulauncher;
in
{
  _file = ./ulauncher.nix;
  options = {
    presets.ulauncher.enable = lib.mkEnableOption "Ulauncher";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      ulauncher

      # Extension dependendies.
      fzf # fzf search
      libqalculate # qalc
    ];

    systemd.user.services.ulauncher = {
      Unit = {
        Description = "Ulauncher service";
        Documentation = "https://ulauncher.io/";
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Type = "simple";
        Restart = "on-failure";
        RestartSec = "2";
        ExecStart = "${pkgs.ulauncher}/bin/ulauncher --hide-window";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
