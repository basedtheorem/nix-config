{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.presets.vscodium;
in
{
  _file = ./vscodium.nix;

  options = {
    presets.vscodium.enable = lib.mkEnableOption "VSCodium";
  };

  config = lib.mkIf cfg.enable {

    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions =
        [ pkgs.vscode-extensions.bbenoist.nix ]
        ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "rainglow";
            publisher = "daylerees";
            version = "1.5.2";
            sha256 = "sha256-1c/xQYnuJ3BkwfqjMeT2kG1ZsXyjEOypJs0pJbouZMQ=";
          }
        ];
      keybindings = [
        {
          "key" = "ctrl+p";
          "command" = "workbench.action.showCommands";
        }
        {
          "key" = "ctrl+shift+p";
          "command" = "-workbench.action.showCommands";
        }
        {
          "key" = "ctrl+o";
          "command" = "workbench.action.quickOpen";
        }
        {
          "key" = "ctrl+p";
          "command" = "-workbench.action.quickOpen";
        }
        {
          "key" = "ctrl+shift+o";
          "command" = "workbench.action.files.openFile";
          "when" = "true";
        }
        {
          "key" = "ctrl+o";
          "command" = "-workbench.action.files.openFile";
          "when" = "true";
        }
        {
          "key" = "ctrl+shift+o";
          "command" = "workbench.action.files.openFolderViaWorkspace";
          "when" = "!openFolderWorkspaceSupport && workbenchState == 'workspace'";
        }
        {
          "key" = "ctrl+o";
          "command" = "-workbench.action.files.openFolderViaWorkspace";
          "when" = "!openFolderWorkspaceSupport && workbenchState == 'workspace'";
        }
        {
          "key" = "ctrl+shift+o";
          "command" = "-workbench.action.gotoSymbol";
        }
        {
          "key" = "ctrl+tab";
          "command" = "-workbench.action.quickOpenNavigateNextInEditorPicker";
          "when" = "inEditorsPicker && inQuickOpen";
        }
        {
          "key" = "ctrl+shift+tab";
          "command" = "-workbench.action.quickOpenNavigatePreviousInEditorPicker";
          "when" = "inEditorsPicker && inQuickOpen";
        }
        {
          "key" = "ctrl+pageup";
          "command" = "-workbench.action.previousEditor";
        }
        {
          "key" = "ctrl+e";
          "command" = "workbench.view.extensions";
          "when" = "viewContainer.workbench.view.extensions.enabled";
        }
        {
          "key" = "ctrl+shift+x";
          "command" = "-workbench.view.extensions";
          "when" = "viewContainer.workbench.view.extensions.enabled";
        }
        {
          "key" = "ctrl+e";
          "command" = "-workbench.action.quickOpen";
        }
        {
          "key" = "ctrl+e";
          "command" = "-workbench.action.quickOpenNavigateNextInFilePicker";
          "when" = "inFilesPicker && inQuickOpen";
        }
        {
          "key" = "shift+f1";
          "command" = "workbench.action.toggleSidebarVisibility";
        }
        {
          "key" = "ctrl+b";
          "command" = "-workbench.action.toggleSidebarVisibility";
        }
        {
          "key" = "ctrl+b";
          "command" = "workbench.action.togglePanel";
        }
        {
          "key" = "ctrl+j";
          "command" = "-workbench.action.togglePanel";
        }
        {
          "key" = "ctrl+0";
          "command" = "-workbench.action.focusSideBar";
        }
        {
          "key" = "shift+f10";
          "command" = "workbench.action.toggleAuxiliaryBar";
        }
        {
          "key" = "ctrl+alt+b";
          "command" = "-workbench.action.toggleAuxiliaryBar";
        }
        {
          "key" = "shift+f10";
          "command" = "-editor.action.showContextMenu";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+t";
          "command" = "-workbench.action.showAllSymbols";
        }
        {
          "key" = "ctrl+k ctrl+pagedown";
          "command" = "-workbench.action.nextEditorInGroup";
        }
        {
          "command" = "-workbench.action.previousEditorInGroup";
          "key" = "ctrl+k ctrl+pageup";
        }
        {
          "key" = "ctrl+k ctrl+left";
          "command" = "-workbench.action.focusLeftGroup";
        }
        {
          "key" = "ctrl+k ctrl+right";
          "command" = "-workbench.action.focusRightGroup";
        }
        {
          "key" = "ctrl+k ctrl+down";
          "command" = "-workbench.action.focusBelowGroup";
        }
        {
          "key" = "ctrl+1";
          "command" = "-workbench.action.focusFirstEditorGroup";
        }
        {
          "key" = "ctrl+1";
          "command" = "workbench.action.firstEditorInGroup";
        }
        {
          "key" = "ctrl+j";
          "command" = "workbench.action.toggleActivityBarVisibility";
        }
        {
          "key" = "ctrl+alt+-";
          "command" = "-workbench.action.navigateBack";
          "when" = "canNavigateBack";
        }
        {
          "key" = "alt+right";
          "command" = "workbench.action.navigateForward";
          "when" = "canNavigateForward";
        }
        {
          "key" = "ctrl+shift+-";
          "command" = "-workbench.action.navigateForward";
          "when" = "canNavigateForward";
        }
        {
          "key" = "ctrl+pageup";
          "command" = "scrollPageDown";
          "when" = "textInputFocus";
        }
        {
          "key" = "alt+pagedown";
          "command" = "-scrollPageDown";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+pagedown";
          "command" = "scrollPageUp";
          "when" = "textInputFocus";
        }
        {
          "key" = "alt+pageup";
          "command" = "-scrollPageUp";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+up";
          "command" = "scrollLineUp";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+up";
          "command" = "-scrollLineUp";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+down";
          "command" = "scrollLineDown";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+down";
          "command" = "-scrollLineDown";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+shift+up";
          "command" = "editor.action.insertCursorAbove";
          "when" = "editorTextFocus";
        }
        {
          "key" = "ctrl+shift+down";
          "command" = "editor.action.insertCursorBelow";
          "when" = "editorTextFocus";
        }
        {
          "key" = "ctrl+shift+pageup";
          "command" = "workbench.action.moveEditorLeftInGroup";
        }
        {
          "key" = "ctrl+shift+pageup";
          "command" = "-workbench.action.moveEditorLeftInGroup";
        }
        {
          "key" = "ctrl+shift+pagedown";
          "command" = "workbench.action.moveEditorRightInGroup";
        }
        {
          "key" = "ctrl+shift+pagedown";
          "command" = "-workbench.action.moveEditorRightInGroup";
        }
        {
          "key" = "f9";
          "command" = "-editor.debug.action.toggleBreakpoint";
          "when" = "debuggersAvailable && editorTextFocus";
        }
        {
          "key" = "f9";
          "command" = "workbench.action.focusActiveEditorGroup";
        }
        {
          "key" = "f9";
          "command" = "editor.action.focusNextCursor";
        }
        {
          "key" = "ctrl+n";
          "command" = "workbench.action.files.newUntitledFile";
        }
        {
          "key" = "ctrl+n";
          "command" = "-workbench.action.files.newUntitledFile";
        }
        {
          "key" = "ctrl+t";
          "command" = "workbench.action.files.newUntitledFile";
        }
        {
          "key" = "shift+alt+up";
          "command" = "editor.action.copyLinesUpAction";
          "when" = "editorTextFocus && !editorReadonly";
        }
        {
          "key" = "shift+alt+down";
          "command" = "editor.action.copyLinesDownAction";
          "when" = "editorTextFocus && !editorReadonly";
        }
        {
          "key" = "ctrl+alt+right";
          "command" = "-workbench.action.moveEditorToNextGroup";
        }
        {
          "key" = "ctrl+alt+left";
          "command" = "-workbench.action.moveEditorToPreviousGroup";
        }
        {
          "key" = "shift+alt+meta+2";
          "command" = "workbench.action.moveEditorToLeftGroup";
        }
        {
          "key" = "shift+alt+meta+1";
          "command" = "workbench.action.moveEditorToAboveGroup";
        }
        {
          "key" = "shift+alt+meta+3";
          "command" = "workbench.action.moveEditorToBelowGroup";
        }
        {
          "key" = "shift+alt+meta+4";
          "command" = "workbench.action.moveEditorToRightGroup";
        }
        {
          "key" = "ctrl+k ctrl+up";
          "command" = "-workbench.action.focusAboveGroup";
        }
        {
          "key" = "alt+meta+1";
          "command" = "workbench.action.focusAboveGroup";
        }
        {
          "key" = "alt+meta+3";
          "command" = "workbench.action.focusBelowGroup";
        }
        {
          "key" = "alt+meta+2";
          "command" = "workbench.action.focusLeftGroup";
        }
        {
          "key" = "alt+meta+4";
          "command" = "workbench.action.focusRightGroup";
        }
        {
          "key" = "ctrl+backspace";
          "command" = "deleteWordPartLeft";
          "when" = "textInputFocus && !editorReadOnly";
        }
        {
          "key" = "ctrl+backspace";
          "command" = "-deleteWordLeft";
          "when" = "textInputFocus && !editorReadOnly";
        }
        {
          "key" = "ctrl+delete";
          "command" = "deleteWordPartRight";
          "when" = "textInputFocus && !editorReadOnly";
        }
        {
          "key" = "ctrl+delete";
          "command" = "-deleteWordRight";
          "when" = "textInputFocus && !editorReadOnly";
        }
        {
          "key" = "ctrl+right";
          "command" = "cursorWordPartRight";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+left";
          "command" = "cursorWordPartLeft";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+alt+left";
          "command" = "cursorWordLeft";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+alt+right";
          "command" = "cursorWordEndRight";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+right";
          "command" = "-cursorWordEndRight";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+left";
          "command" = "-cursorWordLeft";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+shift+right";
          "command" = "cursorWordPartRightSelect";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+shift+left";
          "command" = "cursorWordPartLeftSelect";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+shift+right";
          "command" = "-cursorWordEndRightSelect";
          "when" = "textInputFocus";
        }
        {
          "key" = "ctrl+shift+left";
          "command" = "-cursorWordLeftSelect";
          "when" = "textInputFocus";
        }
        {
          "key" = "shift+alt+l";
          "command" = "addCursorsAtSearchResults";
          "when" = "fileMatchOrMatchFocus && searchViewletVisible";
        }
        {
          "key" = "ctrl+shift+l";
          "command" = "-addCursorsAtSearchResults";
          "when" = "fileMatchOrMatchFocus && searchViewletVisible";
        }
        {
          "key" = "shift+alt+i";
          "command" = "-editor.action.insertCursorAtEndOfEachLineSelected";
          "when" = "editorTextFocus";
        }
        {
          "key" = "alt+n";
          "command" = "editor.action.addSelectionToNextFindMatch";
          "when" = "editorFocus";
        }
        {
          "key" = "ctrl+d";
          "command" = "-editor.action.addSelectionToNextFindMatch";
          "when" = "editorFocus";
        }
        {
          "key" = "shift+alt+pagedown";
          "command" = "editor.action.addCursorsToBottom";
        }
        {
          "key" = "shift+alt+pageup";
          "command" = "editor.action.addCursorsToTop";
        }
        {
          "key" = "ctrl+pagedown";
          "command" = "-workbench.action.nextEditor";
        }
        {
          "key" = "ctrl+tab";
          "command" = "workbench.action.nextEditorInGroup";
        }
        {
          "key" = "ctrl+shift+tab";
          "command" = "workbench.action.previousEditorInGroup";
        }
        {
          "key" = "ctrl+0";
          "command" = "workbench.action.lastEditorInGroup";
        }
        {
          "key" = "ctrl+9";
          "command" = "-workbench.action.lastEditorInGroup";
        }
        {
          "key" = "alt+left";
          "command" = "-workbench.action.terminal.focusPreviousPane";
          "when" = "terminalFocus && terminalHasBeenCreated || terminalFocus && terminalProcessSupported";
        }
        {
          "key" = "alt+right";
          "command" = "-workbench.action.terminal.focusNextPane";
          "when" = "terminalFocus && terminalHasBeenCreated || terminalFocus && terminalProcessSupported";
        }
        {
          "key" = "alt+left";
          "command" = "workbench.action.navigateBack";
        }
        {
          "key" = "ctrl+shift+tab";
          "command" = "-workbench.action.quickOpenLeastRecentlyUsedEditorInGroup";
          "when" = "!activeEditorGroupEmpty";
        }
        {
          "key" = "ctrl+tab";
          "command" = "-workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup";
          "when" = "!activeEditorGroupEmpty";
        }
        {
          "key" = "ctrl+pageup";
          "command" = "-workbench.action.quickOpenLeastRecentlyUsedEditor"; # It's least recently used... Might as well disable.
        }
        {
          "key" = "ctrl+pagedown";
          "command" = "workbench.action.quickOpenPreviousRecentlyUsedEditor";
        }
        {
          "key" = "ctrl+alt+meta+3";
          "command" = "workbench.action.moveActiveEditorGroupDown";
        }
        {
          "key" = "ctrl+k down";
          "command" = "-workbench.action.moveActiveEditorGroupDown";
        }
        {
          "key" = "ctrl+alt+meta+2";
          "command" = "workbench.action.moveActiveEditorGroupLeft";
        }
        {
          "key" = "ctrl+k left";
          "command" = "-workbench.action.moveActiveEditorGroupLeft";
        }
        {
          "key" = "ctrl+alt+meta+4";
          "command" = "workbench.action.moveActiveEditorGroupRight";
        }
        {
          "key" = "ctrl+k right";
          "command" = "-workbench.action.moveActiveEditorGroupRight";
        }
        {
          "key" = "ctrl+alt+meta+1";
          "command" = "workbench.action.moveActiveEditorGroupUp";
        }
        {
          "key" = "ctrl+k up";
          "command" = "-workbench.action.moveActiveEditorGroupUp";
        }
        {
          "key" = "ctrl+alt+f";
          "command" = "editor.action.startFindReplaceAction";
        }
      ];
    };
  };
}
