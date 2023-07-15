{ self, inputs, withSystem, ... }:

let  
  inherit (inputs.nixpkgs.lib) nixosSystem;
  inherit (self) nixosModules; # shared modules
in {
  _file = ./default.nix;

  flake.nixosConfigurations = {
  
    xps = withSystem "x86_64-linux" ({ self', ... }: nixosSystem {
      modules = [
        { nixpkgs.pkgs = self'.legacyPackages; }
        nixosModules.gnome
        ./xps
      ];
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
