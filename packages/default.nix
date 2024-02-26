{ inputs, ... }:
{
  _file = ./default.nix;

  perSystem =
    { system, pkgs, ... }:
    {
      packages = {
        lentenrose = pkgs.callPackage ./lentenrose.nix { };
        espanso = pkgs.callPackage ./espanso.nix {
          inherit (pkgs.darwin.apple_sdk.frameworks)
            AppKit
            Cocoa
            Foundation
            IOKit
            Kernel
            AVFoundation
            Carbon
            QTKit
            AVKit
            WebKit
            ;
        };
      };
    };
}
