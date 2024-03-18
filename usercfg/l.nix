{ pkgs, ... }:

{
  _file = ./default.nix;

  config = {
    profiles = {
      linux-main.enable = true;
      base.enable = true;
      dev.enable = true;
    };

    programs = {
      git = {
        userEmail = "lrns@proton.me";
        userName = "1rns";
      };
    };

    home = {
      username = "l";
      homeDirectory = "/home/l";
      stateVersion = "22.11";

      file.".background-image".source = ./nord-red.jpg;

      #TODO: move some of this to modules
      packages = builtins.attrValues {
        # CLI
        inherit (pkgs)
          ripgrep # `grep` alt.
          grex # Generate regex
          procs # `ps` (process )alt.
          dua # Disk usage
          tokei # Count lines of code
          ruplacer # Find & replace
          sd # Alt. find & replace
          hyperfine # Benchmarking
          file # File info
          fd # Find files
          onefetch # Fetch for git repos
          btop # System monitor
          bandwhich # Network monitor
          tealdeer # TLDR for commands
          ttyper # Typing practice
          bat # `cat` alt.
          wget # Download files
          neofetch # System info
          pipe-rename # `ls | renamer`
          glow # Print .md
          fuc # `rm,cp` -> `rmz,cpz`
          broot # Interactive file tree
          kalker # Calculator
          p7zip # 7z util
          difftastic # `diff` alt
          xclip # Clipboard utils
          cpu-x # Detailed sys info
          viddy # `watch <cmd>` alt.
          killall
          sherlock
          dnsutils
          ;
        inherit (pkgs.bat-extras) batgrep;

        # Desktop
        inherit (pkgs)
          epick # colour picker
          freetube # YT client
          bitwarden # Password manager
          floorp # Firefox fork
          onlyoffice-bin_latest # Office suite
          anki
          planify # Todoist
          cryptomator # Encryption
          gimp
          okular
          ;
        inherit (pkgs.xorg)
          xset # Key delay & repeat rate
          xev # Print xserver events
          ;

        # Media
        inherit (pkgs)
          ffmpeg-full # Video/audio/etc utils
          imagemagick # Image utils
          yt-dlp
          alsa-utils
          ;
      };
    };
  };
}
