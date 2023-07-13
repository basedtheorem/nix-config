{ pkgs, ... }:

{
  users.users = {
    l = {
      isNormalUser = true;
      description = "L";
      extraGroups = [ "networkmanager" "wheel" "video" ];
      shell = pkgs.fish;
      packages = with pkgs; [];
    };
  };
}