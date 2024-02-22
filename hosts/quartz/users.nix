{ pkgs, ... }:
{
  _file = ./users.nix;
  users.users = {
    l = {
      isNormalUser = true;
      description = "L";
      extraGroups = [
        "networkmanager"
        "wheel"
      ];
      shell = pkgs.fish;
    };
  };
}
