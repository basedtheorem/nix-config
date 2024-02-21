{ self, pkgs, inputs, config, lib, ... }:
let
  emacsEnabled = config.programs.emacs.enable;

  initConf = builtins.readFile ./init.el;
  baseConf = builtins.readFile ./base.el;
  editingConf = builtins.readFile ./editing.el;
  meowConf = builtins.readFile ./meow.el;
  keybindsConf = builtins.readFile ./keybinds.el;

  initFile = ''
    This file was automatically generated. Do not modify!

    ${initConf}

    ${baseConf}

    ${editingConf}

    ${meowConf}

    ${keybindsConf}
  '';

  nonMelpaPackages = {
    indent-bars = {
      src = "${inputs.indent-bars.outPath}/indent-bars.el";
      version = inputs.indent-bars.shortRev;
      requiredEmacsPackages = [ ];
    };
  };

  emacsPkg = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    defaultInitFile = true;
    package = pkgs.emacs-git;

    override = final: prev: {
      meow = prev.melpaPackages.meow.overrideAttrs (old: {
        patches = [ ./meow-ctrlf.patch ];
      });
    };
    extraEmacsPackages = ep: [
      ep.treesit-grammars.with-all-grammars
    ] ++ lib.attrSets.mapAttrsToList
      (pname: val:
        ep.trivialBuild {
          inherit pname;
          version = val.version;
          inherit (val) src;
          packageRequires = val.requiredPackages;
        })
      nonMelpaPackages;
  };
in
{
  _file = ./default.nix;

  config = lib.mkIf emacsEnabled {
    programs.emacs.package = lib.mkDefault emacsPkg;
    services.emacs.package = lib.mkDefault emacsPkg;

    nixpkgs.overlays = [ inputs.emacs-overlay.overlays.emacs ];

    # Emacs packages' individual dependencies
    environment.systemPackages = [
      pkgs.ripgrep # Deadgrep
      pkgs.python312 # Ctrlf
    ];

    xdg.configFile = {
      "emacs/early-init.el" = {
        text = self.lib.emacs.genPackage "early-init"
          "Config to run on startup, before the GUI is loaded."
          builtins.readFile ./early-init.el;
      };
    };
  };
}
