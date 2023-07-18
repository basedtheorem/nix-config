{ lib, ... }:

{
  _file = ./common.nix;
  
  # Sets `/etc/nix/nix.conf`.
  nix = {
    extraOptions = lib.mkDefault ''
      experimental-features = nix-command flakes
    '';
    
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 1w";
    };
    
    settings = {
      auto-optimise-store = true; # cli: `nix-store --optimise`
      cores = 6;# 0 means all cpu cores are used for building (non-deterministic!)
      
      # Binary caches
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://devenv.cachix.org"
        "https://hydra.nixos.org"
        "https://helix.cachix.org"
        "https://nix-gaming.cachix.org"
        "https://cache.privatevoid.net"
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs"
        "helix.cachix.org-1:ejp9KQpR1FBI2onstMQ34yogDm4OgU2ru6lIwPvuCVs="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        "cache.privatevoid.net:SErQ8bvNWANeAvtsOESUwVYr2VJynfuc9JRwlzTTkVg="
      ];
    };
  };
}
