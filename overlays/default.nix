{
  flake.overlays = {
    micro = final: prev: {
      micro = prev.micro.overrideAttrs (_: { patches = ./relative-goto.patch; });
    };
  };
}
