{
  lib,
  stdenvNoCC,
  fetchzip,
}:

stdenvNoCC.mkDerivation rec {
  pname = "ClockFace-font";
  version = "bad1107";

  # ClockFace, ClockFaceSolid, ClockFaceRect, ClockFaceRectSolid
  src = fetchzip {
    # Use the 'ttc' files here for a smaller closure size.
    # (Using 'ttf' files gives a closure size about 15x larger, as of November 2021.)
    url = "https://github.com/ocodo/ClockFace-font/archive/bad11070c962d328679e9bfec7769fb920097615.zip";
    hash = "sha256-2+kiG5apYMKI5P1o1ahr9BsV87UVWjSmM8S+Vf5MAO0=";
  };

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype
    cd $src
    cp *.ttf $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    description = "Icon font for displaying the time";
    homepage = "https://github.com/ocodo/ClockFace-font";
    license = licenses.cc-by-nc-30;
    maintainers = [ maintainers."1rns" ];
    platforms = platforms.all;
  };
}
