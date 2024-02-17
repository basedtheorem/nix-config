{
inputs,
pkgs,
...
}:
let
  emacs = pkgs.emacsWithPackagesFromUsePackage {
    package = pkgs.emacs-git;
    extraEmacsPackages = epkgs: with epkgs; [
      use-package
    ];
  };
  in {
    # TODO
    #xdg.configFile."emacs/".source = ../sources/emacs;

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
