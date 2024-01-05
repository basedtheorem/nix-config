{
  programs.git = {
    enable = true;
    diff-so-fancy.enable = true;
    userEmail = "lrns@proton.me";
    userName = "1rns";
    signing = {
      signByDefault = true;
      key = null;
    };
    extraConfig = {
      init.defaultBranch = "dev";
    };
  };
}
