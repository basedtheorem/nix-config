inputs: {
  micro = final: prev: {
    micro = prev.micro.overrideAttrs (_: { patches = ./relative-goto.patch; });
  };

  kakoune-unwrapped = final: prev: {
    kakoune = prev.kakoune-unwrapped.overrideAttrs (_: {
      src = inputs.kakoune;
      patches = [];
    });
  };
}
