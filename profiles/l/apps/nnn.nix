{pkgs, ...}: {
  programs.nnn = {
    enable = true;

    bookmarks = {
      d = "~/dots";
      h = "~/dots/profiles/l";
    };

    plugins.mappings = {
      c = "fzcd";
      f = "finder";
      v = "imgview";
    };

    plugins.src =
      (pkgs.fetchFromGitHub {
        owner = "jarun";
        repo = "nnn";
        rev = "v4.8";
        sha256 = "sha256-QbKW2wjhUNej3zoX18LdeUHqjNLYhEKyvPH2MXzp/iQ=";
      })
      + "/plugins";

    extraPackages = with pkgs; [
      ffmpegthumbnailer
      mediainfo
      sxiv
    ];
  };
}
