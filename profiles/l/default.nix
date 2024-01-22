{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./apps/fish.nix
    ./apps/git.nix
    ./apps/lazygit.nix
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
    ./desktop/cursors.nix

    ./services/picom.nix
    ./services/espanso.nix
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
    skim
    file
    fd
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
    imagemagick_light
    discord

    # Desktop
    gifski
    freetube
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
    logseq # testing
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
    dconf2nix
    nix-index

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  news.display = "silent";
  news.json = lib.mkForce {};
  news.entries = lib.mkForce [];
  # disabledModules = [ "misc/news.nix" ];

  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  home.sessionVariables = {
    EDITOR = "micro"; # this var only works here for some reason
    NIXOS_OZONE_WL = "1"; # enable if on wayland
  };

  systemd.user.sessionVariables = {
    NIXPKGS_ALLOW_UNFREE = 1;
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  xsession.enable = true;
  systemd.user.startServices = true; # fixes warning: 'systemd user session is degraded'

  programs.home-manager.enable = true;
}
