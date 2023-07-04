{ config, pkgs, ... }: {

  imports = [
    ./hardware-configuration.nix
    ./fontconfig.nix
  ];

  # Bootloader
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot/efi";
      grub.efiSupport = true;
      grub.device = "nodev";
      grub.configurationLimit = 10;
    };
    kernelParams = [
      "acpi_rev_override" "mem_sleep_default=deep" "intel_iommu=igfx_off" "nvidia-drm.modeset=1"
    ];
    extraModprobeConfig = ''
      options bbswitch load_state=-1 unload_state=1 nvidia-drm
    '';
    blacklistedKernelModules = [
      "nouveau"
      "rivafb"
      "rivatv"
      "nv"
      "uvcvideo"
    ];
    kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];
  };

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  time.timeZone = "Pacific/Auckland";
  i18n.defaultLocale = "en_NZ.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_NZ.UTF-8";
    LC_IDENTIFICATION = "en_NZ.UTF-8";
    LC_MEASUREMENT = "en_NZ.UTF-8";
    LC_MONETARY = "en_NZ.UTF-8";
    LC_NAME = "en_NZ.UTF-8";
    LC_NUMERIC = "en_NZ.UTF-8";
    LC_PAPER = "en_NZ.UTF-8";
    LC_TELEPHONE = "en_NZ.UTF-8";
    LC_TIME = "en_NZ.UTF-8";
  };


  users.users.l = {
    isNormalUser = true;
    description = "L";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
    packages = with pkgs; [
      (callPackage ./uhk-agent.nix {})
    ];
  };
  programs.fish.enable = true;  
  environment.shells = with pkgs; [ fish ];

  nix = {
    settings.cores = 12;
    settings.experimental-features = [ "nix-command" "flakes" ];
    settings.auto-optimise-store = true; # manual method: `nix-store --optimise`
    extraOptions = "trusted-users = l";
    gc = {
	    automatic = true;
	    dates = "weekly";
	    options = "--delete-older-than 1w";
    };
  };

  
  environment.systemPackages = with pkgs; [
    helix
    intel-gpu-tools
    polkit
    uhk-agent
    gnomeExtensions.just-perfection # hide panel, overview tweakes
    gnomeExtensions.paperwm
    gnomeExtensions.another-window-session-manager
    gnomeExtensions.unite # hide title bars
    gnomeExtensions.pano # clipboard manager
    gnomeExtensions.application-volume-mixer
    gnome.gnome-tweaks
    (graphite-gtk-theme.override {
      wallpapers = true;
      themeVariants = [ "all" ];
    })
    gtk4
    tela-circle-icon-theme
  ];

  services = {
    mullvad-vpn.enable = false;
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

  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;
    keyboard.uhk.enable = true;
    cpu.intel.updateMicrocode = config.hardware.enableRedistributableFirmware;
    nvidia.prime = {
      sync.enable = true;
      intelBusId = "0@0:2:0";
      nvidiaBusId = "1@0:0:0";
    };
    nvidia.modesetting.enable = true;
    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
  };
  
  
  powerManagement.cpuFreqGovernor = "performance";


  system.stateVersion = "20.09";

}
