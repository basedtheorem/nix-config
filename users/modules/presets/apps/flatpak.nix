{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.presets.flatpak;
in
{
  imports = [ inputs.flatpak.homeManagerModules.default ];

  options = {
    presets.flatpak.enable = lib.mkEnableOption "Flatpak";
  };

  config = lib.mkIf cfg.enable {
    services.flatpak.enableModule = true;

    services.flatpak.remotes = {
      "flathub" = "https://dl.flathub.org/repo/flathub.flatpakrepo";
      "flathub-beta" = "https://dl.flathub.org/beta-repo/flathub-beta.flatpakrepo";
    };

    services.flatpak.packages = [ "flathub:app/it.mijorus.smile//stable" ];
  };
}
