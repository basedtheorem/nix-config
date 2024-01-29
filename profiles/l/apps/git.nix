{
  programs.git = {
    enable = true;
    delta.enable = true;
    userEmail = "lrns@proton.me";
    userName = "1rns";

    extraConfig = {
      gpg.format = "ssh";
      init.defaultBranch = "dev";
      user.signingkey = "~/.ssh/id_ed25519.pub";
    };
  };
}
