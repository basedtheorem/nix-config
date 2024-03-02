{
  _file = ./default.nix;

  imports = [
    ./nix.nix # automate garbage collection
    ./boot.nix # bootloader, kernel config
    ./hardware.nix # nvidia, peripherals, cpu, etc.
    ./hardware-configuration.nix # results of hardware scan
    ./services.nix # xserver, bluetooth, pipewire, etc.
    ./users.nix # user groups, default shells
    ./desktop.nix # gnome, etc.
    ./misc.nix # networking, timezone, locales
    ./programs.nix # system-wide packages
    ./fonts.nix
  ];

  config.presets = {
    nix.enable = true;
    gnome.enable = true;
  };
}
