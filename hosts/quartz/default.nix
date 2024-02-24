{
  _file = ./default.nix;

  imports = [
    ./nix.nix # automate garbage collection
    ./boot.nix # bootloader, kernel config
    ./hardware.nix # nvidia, peripherals, cpu, etc.
    ./hardware-configuration.nix # results of hardware scan
    ./services.nix # xserver, bluetooth, pipewire, etc.
    ./users.nix # user groups, default shells
    ./desktop.nix # gnome, system-wide packages
    ./misc.nix # networking, timezone, locales
    ./vm.nix # windows 11 vm
    ./fonts.nix
  ];

  config.presets = {
    base.enable = true;
    dev.enable = true;
  };
}
