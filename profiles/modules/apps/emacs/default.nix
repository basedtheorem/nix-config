{ self, pkgs, inputs, config, lib, ... }:
let
  emacsEnabled = config.programs.emacs.enable;

  "initConf" = builtins.readFile ./init.el;
  "baseConf" = builtins.readFile ./base.el;
  "editingConf" = builtins.readFile ./editing.el;
  "meowConf" = builtins.readFile ./meow.el;
  "keybindsConf" = builtins.readFile ./keybinds.el;

  mergedConf = ''
    ;; This file was automatically generated. Do not modify!

    ${initConf}

    ${keybindsConf}

    ${baseConf}

    ${meowConf}

    ${editingConf}
  '';

  nonMelpaPackages = {
    indent-bars = {
      src = pkgs.writeText
        "indent-bars.el"
        (builtins.readFile "${inputs.indent-bars.outPath}/indent-bars.el");
      version = inputs.indent-bars.shortRev;
      requiredEmacsPackages = ep: [ ep.compat ];
    };
  };

  emacsPkg = pkgs.emacsWithPackagesFromUsePackage {
    config = mergedConf;
    package = pkgs.emacs-unstable;

    override = _: prev: {
      meow = prev.melpaPackages.meow.overrideAttrs (_:
        { patches = [ ./meow-ctrlf.patch ]; }
      );
    };

    extraEmacsPackages = ep:
      [ ep.treesit-grammars.with-all-grammars ]
      ++ lib.attrsets.mapAttrsToList (pname: val:
        ep.trivialBuild {
          inherit pname;
          inherit (val) version src;
          packageRequires = val.requiredEmacsPackages ep;
        })
      nonMelpaPackages;
  };
in
{
  _file = ./default.nix;

  config = lib.mkIf emacsEnabled {
    programs.emacs.package = emacsPkg;
    services.emacs.package = emacsPkg;

    nixpkgs.overlays = [ inputs.emacs-overlay.overlays.emacs ];

    # Emacs packages' individual dependencies.
    home.packages = [
      pkgs.ripgrep # Deadgrep
      pkgs.python312 # Ctrlf
    ];

    xdg.configFile = {
      "emacs/early-init.el".source = ./early-init.el;
      "emacs/init.el".text = mergedConf;
    };
  };
}
