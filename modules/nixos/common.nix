{lib, ...}: {
  _file = ./common.nix;

  nixpkgs.config.allowUnfree = lib.mkDefault true;

  # Sets `/etc/nix/nix.conf`.
  nix = {
    extraOptions = lib.mkDefault ''
      experimental-features = nix-command flakes
    '';

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 3w";
    };

    settings = {
      auto-optimise-store = true; # cli: `nix-store --optimise`
      cores = 6; # 0 means all cpu cores are used for building (non-deterministic!)
      trusted-users = ["@wheel"];

      # Binary caches
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://devenv.cachix.org"
        "https://hydra.nixos.org"
        "https://nix-gaming.cachix.org"
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs"
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
      ];
    };
  };
}
