{ pkgs, ... }:

{
  imports = [
    ./apps/helix.nix
    ./apps/fish.nix
    ./apps/git.nix
    ./apps/vivaldi.nix
    ./apps/lazygit.nix
    
    ./services/picom.nix
    ./services/espanso.nix
    ./services/flameshot.nix
    ./services/xbanish.nix
  ];
  
  home.username = "l";
  home.homeDirectory = "/home/l";
  home.stateVersion = "22.11";

  home.packages = with pkgs; [
    # CLI
    ripgrep
    skim # fzf alt
    file
    fd
    tealdeer
    delta
    bat
    pciutils
    jq
    wget
    neofetch
    kitty
    starship
    exa # ls alt.
    zoxide # cd alt.
    ranger
    glow # .md viewer
    fontpreview
    flameshot # screenshot tool

    # Desktop
    xorg.xset # key repeat delay
    wtype # retrieve window info
    xbanish # hide mouse cursor on type
    # activitywatch # time tracking (broken on vivaldi)
    syncthing
    emote
    uhk-agent # programmable keyboard
   
    # Apps
    vivaldi-ffmpeg-codecs
    anki # spaced repetition
    obsidian # notes
    cryptomator

    # TODO: make this declarative
    vscode-fhs
        
    # Media
    ffmpeg-full
    yt-dlp
    feh # photo viewer
    alsa-utils

    # Dev
    direnv

    # Misc
          
    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  home.file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
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

  xsession.enable = true;
  systemd.user.startServices = true; # fixes warning: 'systemd user session is degraded'
  home.sessionVariables = {
    EDITOR = "hx";
    NIXPKGS_ALLOW_UNFREE = 1;
  };
 
  programs.home-manager.enable = true;
}
