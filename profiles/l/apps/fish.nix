{
  programs.fish = {
    enable = true;

    shellInit = ''
      set NIXPKGS_ALLOW_UNFREE 1
      set -U fish_greeting
      set NNN_FIFO '/tmp/nnn.fifo'
      starship init fish | source
      zoxide init fish | source
      xset r rate 200 65
      bind \b 'backward-kill-word'
      bind \ea 'prevd-or-backward-word'
      bind \ed 'nextd-or-forward-word'
      list_dir
      set -x DIRENV_LOG_FORMAT ""
      direnv hook fish | source
    '';

    shellAbbrs = {
      l = "exa";
      la = "exa -la";
      ll = "exa -l";
      ls = "nnn -de";
      cat = "bat";
      dots = "cd ~/dots/";
      hm = "cd ~/dots/home-manager";
      home = "cd ~/dots/home-manager";
      os = "cd ~/dots/os/";
      apps = "~/dots/home-manager/apps/";
      trash = "cd ~/trash";
      srm = "sudo rm";
      srmr = "sudo rm -r";
      shx = "sudo hx";
      fonts = "fc-list";
      config = "~/.config";
      cd = "z";
      lg = "lazygit";
      fzf = "sk";
      jt = "jot '";
      jot = "jot '";
      unset = "set -e ";
      hx = "micro";
      mc = "micro";
      ic = "micro";
    };

    functions = {
      jot = "echo $argv >> ~/Sync/notes/Jot.md";
      list_dir = {
        body = "if status --is-interactive; pwd; echo ''; exa -F; end;";
        onVariable = "PWD";
      };
      tere = ''
        set --local result (command tere $argv)
        [ -n "$result" ] && cd -- "$result"
      '';

      n = {
        description = "support nnn quit and change directory";
        wraps = "nnn";
        body = ''
	          # Block nesting of nnn in subshells
	          if test -n "$NNNLVL" -a "$NNNLVL" -ge 1
	              echo "nnn is already running"
	              return
	          end
	      
	          # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
	          # If NNN_TMPFILE is set to a custom path, it must be exported for nnn to
	          # see. To cd on quit only on ^G, remove the "-x" from both lines below,
	          # without changing the paths.
	          if test -n "$XDG_CONFIG_HOME"
	              set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
	          else
	              set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
	          end
	      
	          # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
	          # stty start undef
	          # stty stop undef
	          # stty lwrap undef
	          # stty lnext undef
	      
	          # The command function allows one to alias this function to `nnn` without
	          # making an infinitely recursive alias
	          command nnn $argv
	      
	          if test -e $NNN_TMPFILE
	              source $NNN_TMPFILE
	              rm $NNN_TMPFILE
	          end
	        end
        '';
      };
    };
  };
}
