inputs: {
  #TODO: Refactor this

  micro = final: prev: {
    micro = prev.micro.overrideAttrs (_: {patches = ./relative-goto.patch;});
  };

  kakoune-unwrapped = final: prev: {
    kakoune = prev.kakoune-unwrapped.overrideAttrs (_: {
      src = inputs.kakoune;
      patches = [];
    });
  };

  mpv = final: prev: {
    mpv-unwrapped = prev.mpv-unwrapped.overrideAttrs (_: {src = inputs.mpv;});
    mpv = prev.mpv.override {
      scripts = builtins.attrValues {
        inherit
          (prev.mpvScripts)
          autoload
          mpris # use w/ playerctl
          thumbfast
          ;
      };
    };
  };
}
