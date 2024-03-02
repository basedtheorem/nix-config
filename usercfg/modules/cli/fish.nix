{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
let
  cfg = config.presets.fish;
in
{
  _file = ./fish.nix;

  options = {
    presets.fish.enable = lib.mkEnableOption "Fish shell";
  };

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;

      interactiveShellInit = ''
        set fish_greeting
        procs --gen-completion-out fish | source
        xset r rate 185 160
        bind \b 'backward-kill-word'
        bind \cg expand-abbr or self-insert
        bind \ea 'prevd-or-backward-word'
        bind \es 'nextd-or-forward-word'
        bind \cO 'emacsclient --tty --create-frame'
        bind \ectrlshiftz 'redo'
        bind \e\[1\;5C 'forward-word' 'forward-single-char'
        bind \e\[1\;3C 'forward-word' 'forward-single-char'
        set -x DIRENV_LOG_FORMAT ""
        set -x PAGER "less"
        set -x MICRO_TRUECOLOR 1
        direnv hook fish | source
        clear
        # list_dir
      '';

      shellAbbrs = {
        rm = "rm -I";
        notes = "cd ~/notes/";
        dots = "cd ~/nome/";
        hm = "cd ~/nome/home";
        home = "cd ~/nome/home";
        nome = "cd ~/nome";
        sued = "sudo micro";
        sudoed = "sudo micro";
        trash = "cd ~/Trash";
        srm = "sudo rm";
        srmr = "sudo rm -r";

        cpcb = {
          setCursor = true;
          expansion = "% | xclip -selection clipboard";
        };

        jt = {
          setCursor = true;
          expansion = "jot '%'";
        };
        jot = {
          setCursor = true;
          expansion = "jot '%'";
        };
        ee = "e ~/tmp/todo.md";

        # These depend on other packages.
        cdf = {
          setCursor = true;
          expansion = "z (fd --type directory % | fzf)";
        };
        unset = "set -e ";
        gitc = "git commit -S ";
        ff = "fzf";
        gad = "git add .";
        gitst = "git status";
        cat = "bat";
        ps = "procs --sortd CPU --watch-interval 99";
        sed = "sd --preview";
        rup = "ruplacer";
        loc = "tokei";
        time = "hyperfine";
        diff = "difft";
        type = "ttyper -w 10 --language-file ~/Documents/english_200.txt";
        code = "codium";

        gitcmm = {
          setCursor = true;
          expansion = "git commit -S -m '%'";
        };
        gitcm = {
          expansion = "git commit -S -v";
        };
      };

      plugins = with pkgs.fishPlugins; [
        {
          # tide configure --auto --style=Lean --prompt_colors='16 colors' \
          # --show_time='24-hour format' --lean_prompt_height='Two lines' \
          # --prompt_connection=Dotted --prompt_spacing=Sparse \
          # --icons='Few icons' --transient=Yes
          name = "tide";
          src = inputs.fish-tide;
        }
        {
          name = "fzf.fish";
          src = fzf-fish;
        }
        {
          name = "sponge";
          src = sponge;
        }
      ];

      functions = {
        gitz = ''git clone $argv && cd "$(basename "$1" .git)"'';
        mkz = "mkdir $argv && cd $argv";

        jot = ''
          echo "
          $argv" >> ~/tmp/todo.md
        '';

        # List files in CWD when changing dirs.
        # list_dir = {
        # body = "if status --is-interactive; echo ''; ls -F; end;";
        # onVariable = "PWD";
        # };
      };
    };
  };
}
