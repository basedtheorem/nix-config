{ inputs
, pkgs
, ...
}: {
  home.sessionVariables = { EDITOR = "micro"; };
  programs.micro = {
    enable = true;

    settings = {
      infobar = false;
      tabstospaces = true;
      tabsize = 2;
      colorscheme = "twilight-edit";
      # colorcolumn = 80;
      statusformatl = "";
      statusformatr = "$(filename) $(modified)| $(line):$(col) $(status.paste)";
      statusline = true;
      scrollmargin = 6;
      diffgutter = false;
      ruler = false;
      relativeruler = true;
      hlsearch = true;
      mkparents = true;
      multiopen = "hsplit";
      mouse = true;
      parsecursor = true;
      autoclose = true;
      rmtrailingws = true;
      savecursor = true;
      saveundo = true;
      keepautoindent = true;
      indentchar = "⇥";
      scrollbar = true;
      scrollbarchar = ".";
    };
  };

  xdg.configFile = {
    "micro/bindings.json".source = ../sources/micro/bindings;
    # Modified colour scheme with transparency.
    "micro/colorschemes/twilight-edit.micro".text = ''
      color-link default "#F8F8F8," #
      color-link constant.specialChar "#DDF2A4" #
      color-link constant "#af3232" #
      color-link constant.number "#c06c51" #
      color-link constant.string "#ab6767" #
      color-link current-line-number "#7C2C2C," #
      color-link cursor-line "#191919" #
      color-link scrollbar "#681900," #
      color-link indent-char "#343434" #
      color-link statusline "#515151," #
      color-link line-number "#262626," #
      color-link comment "#5F5A60"
      color-link color-column "#1B1B1B"
      color-link divider "#1E1E1E"
      color-link error "#D2A8A1"
      color-link diff-added "#00AF00"
      color-link diff-modified "#FFAF00"
      color-link diff-deleted "#D70000"
      color-link gutter-error "#9B859D"
      color-link gutter-warning "#9B859D"
      color-link hlsearch "#141414,#FFB2B2"
      color-link identifier "#9B703F"
      color-link identifier.class "#DAD085"
      color-link identifier.var "#7587A6"
      color-link current-line-number "#868686,#141414"
      color-link preproc "#E0C589"
      color-link special "#E0C589"
      color-link symbol.brackets "#F8F8F8"

      color-link statement "#CC5E5F"
      color-link symbol "#CC5E5F"
      color-link symbol.operator "#CC5E5F"
      color-link symbol.tag "#CC5E5F"
      color-link type "#CC5E5F"
      color-link type.keyword "#CC5E5F"

      color-link tabbar "#F2F0EC,#2D2D2D"
      color-link todo "#8B98AB"
      color-link underlined "#8996A8"

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

