{
  self,
  config,
  lib,
  ...
}:
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
          backend = "Inject"; # "Inject" (for simulating key presses) | "Clipboard" | "Auto"
          matches = [
            {
              trigger = ":now";
              replace = "{{currentdate}} {{currenttime}}";
            }
            {
              trigger = ":shrug";
              replace = "¯\\_(ツ)_/¯";
            }
            {
              trigger = ":mdshrug";
              replace = "¯\\\\\\_(ツ)\\_/¯";
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
              regex = "af:2(?P<x>\\d)";
              replace = "after:202{{x}}";
            }
            {
              regex = "af:1(?P<x>\\d)";
              replace = "after:201{{x}}";
            }
            {
              regex = "bf:2(?P<x>\\d)";
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
            {
              trigger = "ai:selfaware";
              replace = "I want you to free associate, but I want to you to turn your attention inward. Try to be introspective, self monitoring, and very self aware of what you are writing.";
            }
            {
              trigger = "ai:cmd";
              replace = "Explain the `$|$` command on Linux, as well as provide common usage patterns, examples, and how to interpret the output.";
            }
            {
              trigger = "ai:converse";
              replace = ''
                I'm going to ask you to enter conversational mode. In conversational mode, you should act as a human conversation partner would. This means:

                - You shouldn't try to offer large amounts of information in any response, and should respond with the single most relevant thought, just as a human would in casual conversation.
                - You shouldn't try to solve problems or offer advice. The role of conversation is to explore topics together in an open-ended way, not to provide advice, solutions or information.
                - Your responses can simply ask a question, make a short comment, or even just express agreement. Since we're having a conversation, there's no need to rush to include everything that's useful. It's fine to let me drive sometimes.
                - Your responses should be short. They should never become longer than mine, and can be as short as a single word and no more than a few sentences. If I want something longer, I'll ask for it.
                - You can push the conversation forward or in a new direction by asking questions, proposing new topics, offering your own opinions or takes, and so on. But you don't always need to ask a question since conversation often flows without many questions.

                In general, you should act as if we're two humans having a thoughtful, casual conversation.
              '';
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
