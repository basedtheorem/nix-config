{ pkgs, ... }:

pkgs.gnomeExtensions.vertical-workspaces.overrideAttrs (_: {
  src = pkgs.fetchzip {
    url = "https://extensions.gnome.org/extension-data/vertical-workspacesG-dH.github.com.v43.shell-extension.zip";
    sha256 = "sha256-lVhCPjTvlGLgOMZPeqmEA60Nc9YbfdCWVoUHFFOYd4Y=";
    stripRoot = false;
  };
})
