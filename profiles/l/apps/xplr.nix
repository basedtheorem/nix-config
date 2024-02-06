{ pkgs, ... }: {
  #TODO: Module -> if dua installed, enable dua integration, etc.

  programs.fish.shellAbbrs.xcd = ''cd "$(xplr --print-pwd-as-result)"'';

  programs.xplr = {
    enable = true;

    plugins = {
      # wl-clipboard = fetchFromGitHub {
      #   owner = "sayanarijit";
      #   repo = "wl-clipboard.xplr";
      #   rev = "a3ffc87460c5c7f560bffea689487ae14b36d9c3";
      #   hash = "sha256-I4rh5Zks9hiXozBiPDuRdHwW5I7ppzEpQNtirY0Lcks=";
      # };
      # local-plugin = "/home/user/.config/plugins/local-plugin";
    };

    extraConfig = ''
      xplr.config.modes.builtin.default.key_bindings.on_key.R = {
        help = "batch rename",
        messages = {
          {
            BashExec = [===[
             SELECTION=$(cat "''${XPLR_PIPE_SELECTION_OUT:?}")
             NODES=''${SELECTION:-$(cat "''${XPLR_PIPE_DIRECTORY_NODES_OUT:?}")}
             if [ "$NODES" ]; then
               echo -e "$NODES" | renamer
               "$XPLR" -m ExplorePwdAsync
             fi
           ]===],
          },
        },
      }
    '';
  };
}
