{
inputs,
pkgs,
config,
...
}:
let
  emacs = pkgs.emacsWithPackagesFromUsePackage {
    package = pkgs.emacs-git;
    extraEmacsPackages = epkgs: with epkgs; [
      use-package
    ];
  slink = config.lib.mkOutOfStoreSymlink;
  };
  in {
    xdg.configFile = {
      "emacs/early-init.el" = slink ./early-init.el;
      "emacs/init.el" = slink ./init.el;
      "emacs/lrns.el" = slink ./lrns.el;

      "emacs/base.el" = slink ./base.el;
      "emacs/dev.el" = slink ./dev.el;
      "emacs/meow.el" = slink ./meow.el;
    };

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
