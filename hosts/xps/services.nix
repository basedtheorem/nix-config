{
  services = {
    mullvad-vpn.enable = true;
    fstrim.enable = true;
    thermald.enable = true;
    blueman.enable = true;
    tlp.enable = false;

    pipewire = {
      enable = true;
      alsa.enable = false;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      layout = "us";
      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
      };
      videoDrivers = [ "nvidia" "intel" ];
      config = ''
        Section "Device"
          Identifier  "Intel Graphics"
          Driver      "intel"
          #Option      "AccelMethod"  "sna" # default
          #Option      "AccelMethod"  "uxa" # fallback
          Option      "TearFree"        "true"
          Option      "SwapbuffersWait" "true"
          BusID       "PCI:0:2:0"
          #Option      "DRI" "2"             # DRI3 is now default
        EndSection

        Section "Device"
          Identifier "nvidia"
          Driver "nvidia"
          BusID "PCI:1:0:0"
          Option "AllowEmptyInitialConfiguration"
        EndSection
      '';
      screenSection = ''
        Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
        Option         "AllowIndirectGLXProtocol" "off"
        Option         "TripleBuffer" "on"
      '';
    };
  };
}