{ stdenvNoCC, source, ... }:
stdenvNoCC.mkDerivation (
  self: {
    src = source;

    pname = "micro-autofmt";
    version = source.shortRev;

    phases = [
      "unpackPhase"
      "installPhase"
    ];

    installPhase = ''
      mkdir -p $out/micro-autofmt/help
      cp autofmt.lua $out/micro-autofmt/autofmt.lua
      cp repo.json $out/micro-autofmt/repo.json
      cp help/autofmt.md $out/micro-autofmt/help/autofmt.md
    '';
  }
)
