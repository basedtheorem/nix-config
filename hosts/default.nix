{ self, inputs, ... }:

{
  _file = ./default.nix;

  flake.nixosConfigurations = {
    xps = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      modules = [ ./xps { nixpkgs.pkgs = self.legacyPackages.${system}; } ];# ++ [ builtins.attrValues self.nixosModules ];
    };
  };
}
