{
  programs.git = {
    enable = true;
    diff-so-fancy.enable = true;
    userEmail = "lrns@proton.me";
    userName = "basedtheorem";
    extraConfig = {
      init.defaultBranch = "dev";
    };
  };
}
