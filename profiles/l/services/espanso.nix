{
  services.espanso = {
    enable = true;

    matches = {
      base = {
        matches = [{
            trigger = ":now";
            replace = "It's {{currentdate}} {{currenttime}}";
          } {
            trigger = ":hello";
            replace = "line1\nline2";
          } {
            regex = ":hi(?P<person>.*)\\.";
            replace = "Hi {{person}}!";
          } {
          	trigger = "st:r";
          	replace = "site:reddit.com";
          } {
          	trigger = "st:stat";
          	replace = "site:stats.stackexchange.com";
          } {
          	trigger = "st:gh";
          	replace = "site:github.com";
          }
        ];
      };
      global_vars = {
        global_vars = [
          {
            name = "currentdate";
            type = "date";
            params = {format = "%d/%m/%Y";};
          }
          {
            name = "currenttime";
            type = "date";
            params = {format = "%R";};
          }
        ];
      };
    };
  };
}
