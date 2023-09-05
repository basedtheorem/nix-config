{
  services.espanso = {
    enable = true;

    matches = {
      base = {
        backend = "Clipboard";
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
          	trigger = "st:cv"; # cross-validated
          	replace = "site:stats.stackexchange.com";
          } {
          	trigger = "st:se";
          	replace = "site:stackexchange.com";
          } {
          	trigger = "st:gh";
          	replace = "site:github.com";
          } {
          	trigger = "st:lw";
          	replace = "site:lesswrong.com";
          } {
          	trigger = "ob:p";
          	replace = "`\\begin{proof}`\n$|$\n`\\end{proof}`";
          } {
            trigger = "st:hn";
            replace = "site:news.ycombinator.com";
          } {
            trigger = "st:gr";
            replace = "site:goodreads.com";
          } {
            trigger = "w/o";
            replace = "without ";
          } {
            # it wont work unless formatted like this...
          	trigger = "ob:ac";
			replace =
''
```anki
deck: general
tags:
  - knowledge-base


---
$|$
===

```
'';
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
