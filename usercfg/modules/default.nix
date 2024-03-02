{ self, ... }:
{
  flake.homeModules = {
    presets = {
      imports =
        (self.lib.readNixExpsFrom { path = ./apps; })
        ++ (self.lib.readNixExpsFrom { path = ./cli; })
        ++ (self.lib.readNixExpsFrom { path = ./services; })
        ++ (self.lib.readNixExpsFrom { path = ./emacs; });
    };

    profiles = {
      imports = (self.lib.readNixExpsFrom { path = ./profiles; });
    };
  };
}
