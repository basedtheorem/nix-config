{ self, inputs, withSystem, ... }:

let inherit (inputs.nixpkgs.lib) nixosSystem;

in {
  _file = ./default.nix;

  flake.nixosConfigurations = {
  
    # Dell XPS-15 9570 (main laptop)
    xps = withSystem "x86_64-linux" ({ self', ... }: nixosSystem {
      modules = [
        { nixpkgs.pkgs = self'.legacyPackages; }
        ./xps
        inputs.nixos-hardware.nixosModules.dell-xps-15-9560
      ]
      ++ builtins.attrValues self.nixosModules;
    });

    # TODO
    # pavillion = withSystem "x86_64-linux" ({ self', ... }: nixosSystem {
    #   modules = [
    #     ./pavillion
    #     { nixpkgs.pkgs = self'.legacyPackages; }
    #   ];
    # });  
  };
}