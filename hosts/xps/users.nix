{ pkgs, ... }:

{
  _file = ./users.nix;
  users.users = {
    l = {
      isNormalUser = true;
      description = "L";
      extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
      shell = pkgs.fish;
    };
  };
}
