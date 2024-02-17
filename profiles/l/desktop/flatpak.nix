{inputs, ...}: {
  imports = [inputs.flatpak.homeManagerModules.default];

  services.flatpak.enableModule = true;

  services.flatpak.remotes = {
    "flathub" = "https://dl.flathub.org/repo/flathub.flatpakrepo";
    "flathub-beta" = "https://dl.flathub.org/beta-repo/flathub-beta.flatpakrepo";
  };

  services.flatpak.packages = ["flathub:app/it.mijorus.smile//stable"];
}
