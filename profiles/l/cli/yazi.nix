{
  programs.yazi = {
    enable = true;
    enableFishIntegration = true;

    keymap = {
      input.keymap = [
        {
          exec = "close";
          on = ["<C-q>"];
        }
        {
          exec = "close --submit";
          on = ["<Enter>"];
        }
        {
          exec = "escape";
          on = ["<Esc>"];
        }
        {
          exec = "backspace";
          on = ["<Backspace>"];
        }
      ];
      # manager.keymap = [
      #   { exec = "escape"; on = [ "<Esc>" ]; }
      #   { exec = "quit"; on = [ "q" ]; }
      #   { exec = "close"; on = [ "<C-q>" ]; }
      # ];
    };

    settings = {
      # log = {
      #   enabled = false;
      # };
      # manager = {
      #   show_hidden = false;
      #   sort_by = "modified";
      #   sort_dir_first = true;
      #   sort_reverse = true;
      # };
    };
  };
}