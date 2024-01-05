{
  inputs,
  pkgs,
  ...
}: {
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

    "micro/colorschemes/solarized-tc-edit.micro".text = ''
      # Solarized without the background theme.
      color-link comment "#586E75,#002833"
      color-link identifier "#268BD2,#002833"
      color-link constant "#2AA198,#002833"
      color-link constant.specialChar "#DC322F,#002833"
      color-link statement "#859900,#002833"
      color-link symbol "#859900,#002833"
      color-link preproc "#CB4B16,#002833"
      color-link type "#B58900,#002833"
      color-link special "#268BD2,#002833"
      color-link underlined "#D33682,#002833"
      color-link error "bold #CB4B16,#002833"
      color-link todo "bold #D33682,#002833"
      color-link hlsearch "#002833,#B58900"
      color-link statusline "#586E75,"
      color-link tabbar "#003541,#839496"
      color-link indent-char "#003541,#002833"
      color-link line-number "#586E75,"
      color-link current-line-number "#586E75,"
      color-link diff-added "#3e943e"
      color-link diff-modified "#47b1b5"
      color-link diff-deleted "#944040"
      color-link gutter-error "#003541,#CB4B16"
      color-link gutter-warning "#CB4B16,#002833"
      color-link cursor-line "#1e2842"
      color-link color-column "#003541"
      color-link type.extended "#839496,#002833"
      color-link symbol.brackets "#839496,#002833"
      color-link scrollbar "000510,"
    '';
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
