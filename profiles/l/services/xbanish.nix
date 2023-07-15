{ pkgs, ... }:


{
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
}
