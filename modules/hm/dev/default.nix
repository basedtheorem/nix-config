{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.presets.dev;
in
{
  _file = ./default.nix;

  options.presets.dev = {
    enable = lib.mkEnableOption "Development configuration (direnv, git, etc.)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = builtins.attrValues {
      inherit (pkgs)
        git-filter-repo
        chromium # Web
        jq # JSON query
        tree-sitter

        # `entr -rs <files> <commands>`
        # run commands on file change, -r(eload on each change), -s(hell envvar)
        entr
        ;
    };

    programs = {
      direnv.enable = true;
      direnv.nix-direnv.enable = true;
      lazygit.enable = true;

      git = {
        enable = true;
        delta.enable = true;

        extraConfig = {
          gpg.format = "ssh";
          init.defaultBranch = "dev";
          user.signingkey = "~/.ssh/id_ed25519.pub";
        };
      };
    };
  };
}
