{
  #TODO: try helix again when this is merged: https://github.com/helix-editor/helix/pull/9143
  programs.helix = {
    enable = true;

    settings = {
      editor.cursor-shape.insert = "bar";
      keys.insert = {
        "C-right" = [
          "move_next_word_end"
          "collapse_selection"
          "insert_mode"
          "move_char_right"
        ];
        "C-left" =
          [ "move_prev_word_start" "collapse_selection" "insert_mode" ];
      };
    };
  };
}
