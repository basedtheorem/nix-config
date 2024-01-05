{
  programs.micro = {
  	enable = true;

    settings = {
      infobar = false;
      tabstospaces = true;
      tabsize = 2;
      colorscheme = "solarized-tc-edit";
      statusformatl = "$(filename) $(modified)| $(col) $(status.paste)";
      statusformatr = "";
      scrollmargin = 6;
      diffgutter = true;
      hlsearch = true;
      mkparents = true;
      mouse = true;
      parsecursor = true;
      autoclose = true;
      rmtrailingws = true;
      savecursor = true;
      saveundo = true;
      scrollbar = true;
    };
  };
  xdg.configFile = {
    "micro/bindings.json".source = ../sources/micro/bindings;
  };
}

# # mark/unmark current line (Ctrl-F2)
# > toggleBookmark
#
# # clear all bookmarks (CtrlShift-F2)
# > clearBookmarks
#
# # jump to next bookmark (F2)
# > nextBookmark
#
# # jump to previous bookmark (Shift-F2)
# > prevBookmark
