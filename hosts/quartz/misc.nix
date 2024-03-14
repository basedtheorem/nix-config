{
  _file = ./misc.nix;

  networking = {
    nameservers = [
      "1.1.1.1"
      "9.9.9.9"
    ];
    networkmanager.enable = true;
    networkmanager.dns = "none";
    dhcpcd.extraConfig = "nohook resolv.conf";
    hostName = "quartz";
    firewall = {
      enable = true;
    };
  };

  time.timeZone = "Pacific/Auckland";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
}
