{ pkgs, lib, self, ... }: {
  imports = [
    ./apps/fish.nix
    ./apps/git.nix
    ./apps/gitui.nix
    ./apps/vivaldi.nix
    ./apps/mpv.nix
    # ./apps/xplr.nix #TODO
    ./apps/obsidian.nix
    ./apps/spicetify.nix
    ./apps/sioyek.nix # PDF
    ./apps/discord.nix
    ./apps/vscodium.nix
    ./apps/micro.nix
    ./apps/firefox.nix
    ./apps/kakoune.nix
    ./apps/yazi.nix
    ./apps/kitty.nix

    ./desktop/gnome.nix
    ./desktop/flatpak.nix
    ./desktop/obs.nix

    ./services/picom.nix
    ./services/opensnitch.nix
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
    ruplacer # find and replace
    skim # fzf
    ranger # tui filemgr
    hyperfine # benchmarking tool
    file
    fd
    peco # filtering tool
    onefetch # neofetch but for git repos
    btop # system monitor
    bandwhich # display network utilisation
    git-filter-repo
    socat # pass cmds to mpv socket
    tealdeer
    ttyper # typing practice
    bat
    jq

    wget
    neofetch
    starship
    pipe-rename
    zoxide
    eza
    glow
    fontpreview
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
    imagemagick
    flameshot
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
    nixd
    cachix
    nixfmt

    # `echo "GET <link>" | hurl -o ./out`
    hurl

    # `entr -rs <files> <commands>`
    # run commands on file change, -r(eload on each change), -s(hell envvar)
    entr

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

  nixpkgs = {
    overlays = [ self.overlays.micro ];
    config.allowUnfree = true;
  };

  news.display = lib.mkForce "silent";
  news.json = lib.mkForce { };
  news.entries = lib.mkForce [ ];

  home.file = {
    # ".screenrc".source = dotfiles/screenrc;
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  xsession.enable = true;
  # systemd.user.startServices = true; # fixes warning: 'systemd user session is degraded'
}
