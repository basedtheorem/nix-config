{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./apps/fish.nix
    ./apps/git.nix
    ./apps/gitui.nix
    ./apps/vivaldi.nix
    ./apps/mpv.nix
    ./apps/nnn.nix
    # ./apps/helix.nix
    ./apps/obsidian.nix
    ./apps/spicetify.nix
    ./apps/discord.nix
    ./apps/vscodium.nix
    ./apps/micro.nix
    ./apps/firefox.nix
    ./apps/kakoune.nix
    ./apps/yazi.nix
    ./apps/kitty.nix

    ./desktop/gnome.nix
    ./desktop/flatpak.nix
    ./desktop/cursors.nix

    ./services/picom.nix
    ./services/espanso.nix
    ./services/ulauncher.nix
    ./services/syncthing.nix
    ./services/flameshot.nix
    ./services/xbanish.nix
    ./services/sxhkd.nix
  ];

  home = {
    username = "l";
    homeDirectory = "/home/l";
    stateVersion = "22.11";
  };

  home.packages = with pkgs; [
    # Demo
    tere

    # CLI
    ripgrep
    grex # generate regex
    bat-extras.batgrep
    procs # ps alt
    dua # disk usage
    tokei # sloc
    sd # sed
    skim # fzf
    hyperfine # benchmarking tool
    file
    fd
    onefetch # neofetch but for git repos
    btop # system monitor
    bandwhich # display network utilisation
    git-filter-repo
    socat # pass cmds to mpv socket
    tealdeer
    delta
    bat
    pciutils
    jq
    wget
    neofetch
    starship
    zoxide
    eza
    ranger
    glow
    fontpreview
    peco
    sic-image-cli
    broot # interactive tree
    kalker
    p7zip
    difftastic

    # Desktop
    gifski
    planify
    gcolor3 # colour picker
    freetube
    bitwarden
    xorg.xset
    xclip
    flameshot
    freetube
    gimp
    qbittorrent
    xbanish
    chromium
    lite-xl
    uhk-agent
    cpu-x

    vivaldi-ffmpeg-codecs
    anki
    cryptomator
    okular
    qalculate-gtk
    syncthing
    libreoffice
    woeusb-ng

    # Media
    ffmpeg-full
    yt-dlp
    alsa-utils
    playerctl

    # Dev
    just
    xorg.xev
    nix-index

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  programs = {
    direnv.enable = true;
    direnv.nix-direnv.enable = true;
    home-manager.enable = true;
  };

  news.display = "silent";
  news.json = lib.mkForce {};
  news.entries = lib.mkForce [];

  home.file = {
    # ".screenrc".source = dotfiles/screenrc;
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  home.sessionVariables = {
    EDITOR = "micro";
    # NIXOS_OZONE_WL = "1"; # #TODO: auto enable if on wayland
  };

  systemd.user.sessionVariables = {
    NIXPKGS_ALLOW_UNFREE = 1;
  };

  xsession.enable = true;
  systemd.user.startServices = true; # fixes warning: 'systemd user session is degraded'
}
