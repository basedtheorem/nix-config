{ self, inputs, withSystem, ... }:

let inherit (inputs.nixpkgs.lib) nixosSystem;

in {
  _file = ./default.nix;

  flake.nixosConfigurations = {
  
    xps = withSystem "x86_64-linux" ({ self', ... }: nixosSystem {
      modules = [
        { nixpkgs.pkgs = self'.legacyPackages; }
        ./xps
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
