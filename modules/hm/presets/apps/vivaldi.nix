{ config, lib, ... }:
let
  cfg = config.presets.vivaldi;
in
{
  options = {
    presets.vivaldi.enable = lib.mkEnableOption "Vivaldi browser";
  };

  config = lib.mkIf cfg.enable {
    programs.vivaldi = {
      enable = true;

      commandLineArgs = [
        # "--enable-gpu-rasterization"
        # "--enable-oop-rasterization"
        # "--enable-gpu-compositing"
        # "--enable-accelerated-2d-canvas"
        #"--enable-zero-copy"
        # "--enable-unsafe-webgpu"
        # "--ignore-gpu-blocklist"
        # "--use-gl=desktop"
        # "--use-vulkan"
        # "--canvas-oop-rasterization"
        # "--disable-features=UseChromeOSDirectVideoDecoder,UseSkiaRenderer"
        # "--enable-accelerated-video-decode"
        # "--enable-accelerated-video-encode"
        # "--enable-features=VaapiVideoDecoder"
        # "--enable-hardware-overlays"
      ];
    };
  };
}
