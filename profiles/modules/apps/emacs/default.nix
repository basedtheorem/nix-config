{
  pkgs,
  inputs,
  config,
  lib,
  ...
}:
let
  emacsEnabled = config.programs.emacs.enable;

  "initConf" = builtins.readFile ./init.el;
  "keybindsConf" = builtins.readFile ./keybinds.el;
  "baseConf" = builtins.readFile ./base.el;
  "devConf" = builtins.readFile ./dev.el;
  "meowConf" = builtins.readFile ./meow.el;
  "editingConf" = builtins.readFile ./editing.el;

  mergedConf = ''
    ;; This file was automatically generated. Do not modify!

    ${initConf}

    ${keybindsConf}

    ${baseConf}

    ${devConf}

    ${editingConf}

    ${meowConf}
  '';

  nonMelpaPackages = {
    indent-bars = {
      src = pkgs.writeText "indent-bars.el" (
        builtins.readFile "${inputs.indent-bars.outPath}/indent-bars.el"
      );
      version = inputs.indent-bars.shortRev;
      requiredEmacsPackages = ep: [ ep.compat ];
    };
  };

  emacsPkg = pkgs.emacsWithPackagesFromUsePackage {
    config = mergedConf;
    package = pkgs.emacs-unstable;

    override = _: prev: {
      meow = prev.melpaPackages.meow.overrideAttrs (_: { patches = [ ./meow-ctrlf.patch ]; });
    };

    extraEmacsPackages =
      ep:
      [ ep.treesit-grammars.with-all-grammars ]
      ++
        lib.attrsets.mapAttrsToList
          (
            pname: val:
            ep.trivialBuild {
              inherit pname;
              inherit (val) version src;
              packageRequires = val.requiredEmacsPackages ep;
            }
          )
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

    programs.fish.functions = {
      e = ''
        if test (count $argv) -ge 1 && test $argv[1] = -
           set tempfile "$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)"
           cat - > "$tempfile"
           emacsclient -c -t --eval "(find-file \"$tempfile\")" \
                       --eval '(set-visited-file-name nil)' \
                       --eval '(rename-buffer "*stdin*" t)' \
                       --eval "(setq default-directory \"$PWD/\")"
        else
           emacsclient -c -t $argv
        end
      '';
      ed = "e $argv";
    };
  };
}
