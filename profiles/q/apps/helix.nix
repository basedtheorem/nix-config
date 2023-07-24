{ pkgs, ... }: {

  programs.helix = {
    enable = true;
    defaultEditor = true;
 
    settings = {
      theme = "amberwood";
      editor = {
        line-number = "relative";
        lsp.display-messages = true;
        cursor-shape.insert = "bar";
        shell = [ "fish" ];
        indent-guides = {
          render = true;
        };
      };

      keys.normal = {
        "l" = "insert_mode";
        "k" = "undo";
        "u" = "move_line_up";
        "n" = "move_char_left";
        "e" = "move_line_down";
        "i" = "move_char_right";
        "h" = "collapse_selection";
        "A-h" = "flip_selections";
        "j" = "search_next";

        "A-n"  = "select_prev_sibling";
        "A-e"  = "shrink_selection";
        "A-i"  = "expand_selection";
        "A-o" = "select_next_sibling";
      };

      keys.normal.g = {
        "n" = "goto_line_start";
        "o" = "goto_line_end";
      };

      keys.normal.space.w = {
        "n" = "jump_view_left";
        "e" = "jump_view_down";
        "u" = "jump_view_up";
        "i" = "jump_view_right";
      };
     
      keys.normal.C-w = {
        "n" = "jump_view_left";
        "e" = "jump_view_down";
        "u" = "jump_view_up";
        "i" = "jump_view_right";

        "E" = "join_selections";
        "A-E" = "join_selections_space";
        "I" = "keep_selections";
        "A-I" = "remove_selections";
      };
      
      keys.insert = {
        "C-backspace" = "delete_word_backward";
        "A-x" = "normal_mode";
        "C-left" = "move_prev_word_start";
        "C-right" = "move_next_word_start";
        "C-f" = "search";
      };

      keys.select = {
        "n" = "move_char_left";
        "e" = "move_line_down";
        "u" = "move_line_up";
        "i" = "move_char_right";

        "h" = "search_next";
        "H" = "search_prev";
        "j" = "move_next_word_end";
        "J" = "move_next_long_word_end";
        "k" = "undo";
        "K" = "redo";
        "l" = "insert_mode";
        "L" = "insert_at_line_start";
      };

      keys.normal.z = {
        "e" = "scroll_down";
        "u" = "scroll_up";
      };
      
      keys.normal.Z = {
        "e" = "scroll_down";
        "u" = "scroll_up";
      };
      
    };
  };
}
