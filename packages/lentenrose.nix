{
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  pname = "LentenRose";
  description = "LentenRose SDDM Theme.";
  version = "849a5f4";
  dontBuild = true;

  src = fetchFromGitHub {
    owner = "TheoIsDumb";
    repo = "LentenRose";
    rev = "849a5f4d8386611551a51f4ba16566ae9ef4dee9";
    sha256 = "sha256-acanCj9LMS6xYjw+ZUKVW3RLvN1PJC4Dyjnv1Ri+R98=";
  };

  installPhase = ''
    mkdir -p $out/share/sddm/themes
    cp -aR $src $out/share/sddm/themes/LentenRose
  '';
}
