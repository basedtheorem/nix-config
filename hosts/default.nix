{ inputs, ... }:

{
  flake.nixosConfigurations = {
    xps = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./xps ];
    };
  };
}