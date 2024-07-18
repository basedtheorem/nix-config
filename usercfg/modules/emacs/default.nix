{
  pkgs,
  inputs,
  config,
  lib,
  ...
}:
let
  cfg = config.presets.emacs;

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
    man-preview = {
      src = pkgs.writeText "man-preview.el" (builtins.readFile ./man-preview.el);
      version = "0.0.0";
    };
  };

  emacsPkg = pkgs.emacsWithPackagesFromUsePackage {
    config = mergedConf;
    package = pkgs.emacs-unstable;

    override = _: prev: {
      meow = prev.melpaPackages.meow.overrideAttrs (_: {
        patches = [ ./meow-ctrlf.patch ];
      });
    };

    extraEmacsPackages =
      ep:
      [ ep.treesit-grammars.with-all-grammars ]
      ++ lib.attrsets.mapAttrsToList (
        pname: val:
        ep.trivialBuild {
          inherit pname;
          inherit (val) version src;
          packageRequires =
            if builtins.hasAttr "requiredEmacsPackages" val then val.requiredEmacsPackages ep else [ ];
        }
      ) nonMelpaPackages;
  };
in
{
  _file = ./default.nix;

  options = {
    presets.emacs.enable = lib.mkEnableOption "emacs";
  };

  config = lib.mkIf cfg.enable {
    programs.emacs.package = emacsPkg;
    programs.emacs.enable = true;
    services.emacs = {
      package = emacsPkg;
      enable = true;
      defaultEditor = true;
    };

    nixpkgs.overlays = [ inputs.emacs-overlay.overlays.emacs ];

    # Emacs packages' individual dependencies.
    home.packages = [
      pkgs.ripgrep # Deadgrep
      pkgs.python313 # Ctrlf
      pkgs.ispell # Flyspell mode
      pkgs.multimarkdown # Markdown-mode
    ];

    xdg.configFile = {
      "emacs/early-init.el".source = ./early-init.el;
      "emacs/init.el".text = mergedConf;
    };

    programs.fish = lib.mkIf config.programs.fish.enable {
      # Create separate emacs daemons for each shell.
      interactiveShellInit = ''
        source ${config.xdg.configHome}/fish/functions/__kill_emacs_d__.fish
        set DAEMON_ID (date +"%N") # Current date in nanoseconds
        emacs --daemon="$DAEMON_ID" &> /dev/null &;
      '';

      functions =
        let
          emacsClientSocket = ''emacsclient --create-frame --tty --socket-name "$DAEMON_ID"'';
        in
        {
          # View man pages using emacs.
          nam = ''
            emacsclient -s $DAEMON_ID --tty --create-frame \
            --eval \
               "(progn (man \"$argv\") (switch-to-buffer \"*Man $argv*\" t t) (delete-other-frames))";
          '';

          # Kill emacs daemon on exit.
          "__kill_emacs_d__" = {
            onEvent = "fish_exit";
            body = ''
              emacsclient -s "$DAEMON_ID" -e "(kill-emacs)"
            '';
          };

          # Allow emacs to read files from STDIN (e.g. `echo 123 | e -`)
          e = ''
            if test (count $argv) -ge 1 && test $argv[1] = -
                set tempfile "$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)"
                cat - > "$tempfile"
                ${emacsClientSocket} --eval "(find-file \"$tempfile\")" \
                    --eval '(set-visited-file-name nil)' \
                    --eval '(rename-buffer "*stdin*" t)' \
                    --eval "(setq default-directory \"$PWD/\")"
            else
                ${emacsClientSocket} $argv
            end
          '';

          ed = "e $argv";
        };
    };
  };
}
