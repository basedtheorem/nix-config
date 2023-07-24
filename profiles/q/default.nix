{ pkgs, ... }:

{
  imports = [
    ./apps/helix.nix
    ./apps/fish.nix
    ./apps/git.nix
    ./apps/lazygit.nix
  ];
  
  home = {
    username = "q";
    homeDirectory = "/home/q";
    stateVersion = "23.05";
  };

  home.packages = with pkgs; [
    ripgrep
    skim # fzf alt
    file
    fd
    tealdeer
    delta
    bat
    jq
    helix
    wget
    neofetch
    kitty
    starship
    exa # ls alt.
    zoxide # cd alt.
    ranger
    glow # .md viewer
  
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  home.sessionVariables = {
    EDITOR = "hx";
    NIXPKGS_ALLOW_UNFREE = 1;
  };
 
  programs.home-manager.enable = true;
}
