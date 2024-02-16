{ pkgs, ... }: {
  services.espanso = {
    enable = true;

    configs = {
      default = {
        # toggle_key = "RIGHT_CTRL";
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
            regex = ":hi(?P<person>.*)\\.";
            replace = "Hi {{person}}!";
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
            trigger = "af:";
            replace = "after:";
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
            params = { format = "%d/%m/%Y"; };
          }
          {
            name = "currenttime";
            type = "date";
            params = { format = "%R"; };
          }
        ];
      };
    };
  };
}
