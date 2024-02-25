{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.presets.obsidian;
in
{
  options = {
    presets.obsidian.enable = lib.mkEnableOption "Obsidian";
  };

  config = lib.mkIf cfg.enable {
    #TODO: https://github.com/NixOS/nixpkgs/pull/282595
    home.packages = [
      (pkgs.obsidian.overrideAttrs (
        final: prev: {
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            makeWrapper ${pkgs.electron_28}/bin/electron $out/bin/obsidian \
              --set LD_LIBRARY_PATH "${pkgs.lib.makeLibraryPath [ pkgs.libGL ]}" \
              --add-flags $out/share/obsidian/app.asar \
              --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform=wayland}}"
            install -m 444 -D resources/app.asar $out/share/obsidian/app.asar
            install -m 444 -D resources/obsidian.asar $out/share/obsidian/obsidian.asar
            install -m 444 -D "${prev.desktopItem}/share/applications/"* \
              -t $out/share/applications/
            for size in 16 24 32 48 64 128 256 512; do
              mkdir -p $out/share/icons/hicolor/"$size"x"$size"/apps
              convert -background none -resize "$size"x"$size" ${prev.icon} $out/share/icons/hicolor/"$size"x"$size"/apps/obsidian.png
            done
            runHook postInstall
          '';
        }
      ))
    ];
  };
}
