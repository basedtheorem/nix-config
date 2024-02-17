{
  pkgs,
  inputs,
  ...
}: {
  home.packages = [pkgs.kakoune];
  #TODO: put kak dotfiles here
  #   programs.kakoune = {
  #     enable = true;
  #
  #     config = { colorScheme = "desertex"; };
  #   };
}
