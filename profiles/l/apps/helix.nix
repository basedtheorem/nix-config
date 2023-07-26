{
  programs.helix = {
    enable = true;
    defaultEditor = true;
 
    settings = {
      theme = "amberwood";
      editor = {
        line-number = "relative";
        lsp.display-messages = true;
        cursor-shape.insert = "bar";
        cursorline = true;
        scrolloff = 10;
        bufferline = "multiple";
        shell = [ "fish" ];
        indent-guides = {
          render = true;
        };
      };

      keys = {
        normal = {
          "n" = "move_char_left";
          "N" = "keep_selections";
          "k" = "search_next";
          "K" = "search_prev";
        
          "e" = "move_line_down";
          "E" = "join_selections";
          "j" = "move_next_word_end";
          "J" = "move_next_long_word_end";
        
          "u" = "move_line_up";
          "U" = "join_selections";
          "l" = "undo";
          "L" = "redo";
        
          "i" = "move_char_right";
          "I" = "no_op";
          "h" = "insert_mode";
          "H" = "insert_at_line_start";

          C-n = "jump_view_left";
          C-e = "jump_view_down";
          C-u = "jump_view_up";
          C-i = "jump_view_right";

          X = [ "extend_line_up" "extend_to_line_bounds" ];
          esc = [ "collapse_selection" "keep_primary_selection" ];
          home = "goto_first_nonwhitespace";
          C-s = ":w";
          C-w = ":q";
        };

        normal.C-q = {
          "n" = "jump_view_left";
          "e" = "jump_view_down";
          "u" = "jump_view_up";
          "i" = "jump_view_right";

          "E" = "join_selections";
          "A-E" = "join_selections_space";
          "I" = "keep_selections";
          "A-I" = "remove_selections";
        };

        normal.space = {
          f = "file_picker";
          F = "file_picker_in_current_directory";
        };      

        normal.g = {
          "n" = "goto_line_start";
          "o" = "goto_line_end";
        };

        normal.space.w = {
          "n" = "jump_view_left";
          "e" = "jump_view_down";
          "u" = "jump_view_up";
          "i" = "jump_view_right";
        };
     
        insert = {
          "esc" = [ "collapse_selection" "normal_mode" ];
          "C-backspace" = "delete_word_backward";
          "A-x" = "normal_mode";
          "C-left" = "move_prev_word_start";
          "C-right" = "move_next_word_start";
          "C-f" = "search";
          home = "goto_first_nonwhitespace";
          C-s = ":w";
          C-q = ":q";
        };

        select = {
          esc = [ "collapse_selection" "keep_primary_selection" "normal_mode" ];

          "n" = "extend_char_left";
          "e" = [ "extend_line_down" "extend_to_line_bounds" ];
          "u" = [ "extend_line_up" "extend_to_line_bounds" ];
          "i" = "extend_char_right";
          
          home = "extend_to_first_nonwhitespace";
          C-s = ":w";
          C-q = ":q";

          "h" = "search_next";
          "H" = "search_prev";
          "j" = "move_next_word_end";
          "J" = "move_next_long_word_end";
          "k" = "undo";
          "K" = "redo";
          "l" = "insert_mode";
          "L" = "insert_at_line_start";
        };
      };
    };
  };
}
