{
  programs.vivaldi = {
    enable = true;

    commandLineArgs = [
      "--enable-gpu-rasterization"
      "--enable-oop-rasterization"
      "--enable-gpu-compositing"
      "--enable-accelerated-2d-canvas"
      "--enable-zero-copy"
      "--enable-unsafe-webgpu"
      "--use-vulkan"
      "--canvas-oop-rasterization"
      "--disable-features=UseChromeOSDirectVideoDecoder,UseSkiaRenderer"
      "--enable-accelerated-video-decode"
      "--enable-accelerated-video-encode"
      "--enable-features=VaapiVideoDecoder,VaapiVideoEncoder,VaapiIgnoreDriverChecks,VaapiVideoDecodeLinuxGL"
      "--enable-hardware-overlays"
    ];
  };
}
