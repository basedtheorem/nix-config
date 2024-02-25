{ config, lib, ... }:
let
  cfg = config.presets.espanso;
in
{
  _file = ./espanso.nix;

  options = {
    presets.espanso.enable = lib.mkEnableOption "Espanso (text expander)";
  };

  config = lib.mkIf cfg.enable {

    services.espanso = {
      enable = true;

      configs = {
        default = {
          # toggle_key = "RIGHT_CTRL";
          show_notifications = false;
        };
      };

      matches = {
        base = {
          backend = "Clipboard"; # "Inject" (for simulating key presses) | "Clipboard" | "Auto"
          matches = [
            {
              trigger = ":now";
              replace = "{{currentdate}} {{currenttime}}";
            }
            {
              trigger = ":time";
              replace = "{{currenttime}}";
            }
            {
              trigger = ":date";
              replace = "{{currentdate}}";
            }
            {
              regex = ":hi(?P<person>.*)\\.";
              replace = "Hi {{person}}!";
            }
            {
              regex = ":af2(?P<x>\\d)";
              replace = "after:202{{x}}";
            }
            {
              regex = ":af1(?P<x>\\d)";
              replace = "after:201{{x}}";
            }
            {
              regex = ":bf2(?P<x>\\d)";
              replace = "before:202{{x}}";
            }
            {
              trigger = "st:r";
              replace = "site:reddit.com";
            }
            {
              trigger = "st:cv"; # cross-validated
              replace = "site:stats.stackexchange.com";
            }
            {
              trigger = "st:se";
              replace = "site:stackexchange.com";
            }
            {
              trigger = "st:so";
              replace = "site:stackoverflow.com";
            }
            {
              trigger = "st:gh";
              replace = "site:github.com";
            }
            {
              trigger = "st:lw";
              replace = "site:lesswrong.com";
            }
            {
              trigger = "st:hn";
              replace = "site:news.ycombinator.com";
            }
            {
              trigger = "st:gr";
              replace = "site:goodreads.com";
            }
            {
              trigger = "w/o";
              replace = "without ";
            }
          ];
        };

        global_vars = {
          global_vars = [
            {
              name = "currentdate";
              type = "date";
              params = {
                format = "%d/%m/%Y";
              };
            }
            {
              name = "currenttime";
              type = "date";
              params = {
                format = "%R";
              };
            }
          ];
        };
      };
    };
  };
}
