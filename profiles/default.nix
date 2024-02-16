{ self
, inputs
, ...
}:
let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in
{
  flake.homeConfigurations = {
    l = homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
      extraSpecialArgs = {
        inherit inputs;
        inherit self;
      };
      modules = [ ./l ./shared ];
    };
  };
}
