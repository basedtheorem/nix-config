{ pkgs, ... }:

pkgs.gnomeExtensions.vertical-workspaces.overrideAttrs (_: {
  src = pkgs.fetchzip {
    url = "https://extensions.gnome.org/extension-data/vertical-workspacesG-dH.github.com.v37.shell-extension.zip";
    sha256 = "sha256-ZS4DsyJHeak+9IMrB6JrDskR+wIKYO85uxqXTycnF48=";
    stripRoot = false;
  };
})
