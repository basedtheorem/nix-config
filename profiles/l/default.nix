{ pkgs, ... }:

{
  imports = [
    ./apps/helix.nix
    ./apps/fish.nix
    ./apps/git.nix
    ./apps/lazygit.nix
    ./apps/vivaldi.nix
    ./apps/mpv.nix

    ./desktop/gnome.nix

    ./services/picom.nix
    ./services/espanso.nix
    ./services/flameshot.nix
    ./services/xbanish.nix
    ./services/sxhkd.nix
  ];

  home = {
    username = "l";
    homeDirectory = "/home/l";
    stateVersion = "22.11";

    # Disable (VERY) annoying hardcoded gtk keybinds
    file.".profile".text = ''
      GTK_IM_MODULE="xim"
    '';
  };

  home.packages = with pkgs; [
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
    kitty
    starship
    exa
    zoxide
    ranger
    glow
    fontpreview
    xdotool
    flameshot

    # Desktop
    xorg.xset
    wtype
    xbanish
    # activitywatch # time tracking (broken on vivaldi)
    syncthing
    uhk-agent

    # Apps
    vivaldi-ffmpeg-codecs
    anki
    obsidian # TODO: put config in HM
    cryptomator
    geogebra6
    okular
    qalculate-gtk
    vscode-fhs # TODO: make this declarative

    # Media
    ffmpeg-full
    yt-dlp
    feh
    alsa-utils
    playerctl


    # Misc
    dconf2nix

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  home.file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

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

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  xsession.enable = true;
  systemd.user.startServices = true; # fixes warning: 'systemd user session is degraded'
  home.sessionVariables = {
    EDITOR = "hx";
    NIXPKGS_ALLOW_UNFREE = 1;
  };

  programs.home-manager.enable = true;
}
