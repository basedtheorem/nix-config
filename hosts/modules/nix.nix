{ lib, ... }:
{
  _file = ./nix.nix;

  options.presets.nix.enable = lib.mkEnableOption "Nix preset";

  config = {
    nixpkgs.config.allowUnfree = true;

    # Sets `/etc/nix/nix.conf`.
    nix = {
      extraOptions = ''
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
        trusted-users = [ "@wheel" ];

        # Binary caches
        substituters = [
          "https://cache.nixos.org"
          "https://devenv.cachix.org"
          "https://nix-community.cachix.org"
          # "https://nix-gaming.cachix.org"
        ];

        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          # "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        ];
      };
    };
  };
}
