{ inputs, pkgs, ... }: {

  services.playerctld.enable = true; # Send keybinds to mpv

  programs.mpv =
    let
      mpv-git-unwrapped = pkgs.mpv-unwrapped.overrideAttrs (_: { src = inputs.mpv; });
      mpv-git = pkgs.wrapMpv mpv-git-unwrapped { };
    in
    {
      enable = true;

      package = mpv-git.override {
        scripts = builtins.attrValues {
          inherit (pkgs.mpvScripts)
            autoload# Load playlist entries automatically
            mpris# For use with playerctld
            thumbfast# Thumbnails on seek
            ;
        };
      };

      config = {
        # -- Behaviour --
        pause = true;
        border = "no";
        input-ipc-server = "/tmp/mpvsocket"; # for kb cmds that playerctl cant handle
        save-position-on-quit = true;
        resume-playback = false;
        no-keepaspect-window = ""; # breaks paperwm otherwise

        # -- Hardware --
        hwdec = "auto-safe";
        vo = "gpu";
        #profile = "gpu-hq";
        hdr-compute-peak = false;

        # -- yt-dlp --
        ytdl-raw-options = "sub-lang='en',write-subs=,write-auto-sub=,write-subs=";
        ytdl-format = "bestvideo[height<=1440]+bestaudio/best[height<=1440]";

        # -- Subtitles --
        slang = "en,eng";
        alang = "en";
        ass-use-margins = "";
        blend-subtitles = false; # required false for 'ass-use-margins'
        sub-color = "#ffffffff";
        sub-shadow-color = "#ff000000";
        sub-back-color = "#aa000000";
        sub-shadow-offset = 2.5;
        sub-font = "Liberation Sans";
        sub-bold = "yes";
        sub-italic = "no";
        sub-font-size = 28;
        sub-border-size = 0.75;
        sub-border-color = "#ff0a0a0a";
        sub-spacing = 0.5;
        sub-justify = "left";
        sub-pos = 100; # sub pos 0% above bottom of screen
        sid = 1;
        sub-scale = 1;
        sub-ass = "";
        sub-ass-line-spacing = 5;
        embeddedfonts = "";
        sub-scale-by-window = "no";

        # -- On-screen display --
        osc = "no";
        osd-bar = "no";
        osd-bar-align-y = 0.92;
        cursor-autohide = 1500;
        osd-on-seek = "msg";
        osd-duration = 800;
        osd-status-msg = "\${playback-time} | \${percent-pos}%";
        osd-font-size = 28;
        osd-border-size = 1.5;
        background = "#000000";
      };

      scriptOpts = {
        osc = {
          vidscale = true;
          timetotal = false;
          hidetimeout = 500;
          boxalpha = 25;
        };
        ytdl_hook = { ytdl_path = "yt-dlp"; };
      };
    };

  # Additional scripts that aren't in nixpkgs.
  xdg.configFile = {
    "mpv/scripts/oscc.lua".source = ./scripts/oscc;
    "mpv/scripts/seek-show.js".source = ./scripts/seek-show-position;
    "mpv/fonts/".source = ./fonts;
  };
}
