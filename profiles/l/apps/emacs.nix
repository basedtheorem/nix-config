{ inputs
, pkgs
, ...
}:
let
  emacsWithPackages = pkgs.emacsWithPackagesFromUsePackage {
    package = pkgs.emacs-git;
    config = ../sources/init.el;
    defaultInitFile = true;
  };
in
{
  # https://web.archive.org/web/20240209082003/https://esrh.me/posts/2021-11-27-emacs-config
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-git;
  };
  services.emacs = {
    enable = true;
    package = pkgs.emacs-git;
    startWithUserSession = true;
  };
}
