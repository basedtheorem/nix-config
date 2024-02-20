{ self, pkgs, inputs, config, lib, ... }:
let
  cfg = config.programs.emacs;
  confPackages =
    let
      fileContent =
        lib.attrsets.mapAttrs'
          (k: v: {
            name = "${k}";
            value = {
              ep = v.packageRequires;
              src =
                config.lib.emacs.generatePackage k v.tag v.comments v.requires
                  v.code;
            };
          })
          cfg.localPackages;
      derivations =
        lib.attrsets.mapAttrs
          (k: v: {
            inherit (v) ep;
            src = pkgs.writeText "${k}.el" v.src;
          })
          fileContent;
    in
    derivations;

  requires =
    let
      names = lib.attrsets.mapAttrsToList (n: _: n) cfg.localPackages;
      sorted = builtins.sort (l: r: l < r) names;
      required = builtins.map (r: "(require '${r})") sorted;
    in
    builtins.concatStringsSep "\n" required;
in
{
  _file = ./default.nix;

  options.programs.emacs = {
    localPackages = lib.mkOption {
      description = "Use this to modularise packages.";
      type = lib.type.attrsOf
        (lib.types.submodule (_: {
          options = {
            tag = lib.mkOption { type = lib.types.str; };
            comments = lib.mkOption { type = lib.types.listOf lib.types.str; };
            requires = lib.mkOption { type = lib.types.listOf lib.types.str; };
            code = lib.mkOption { type = lib.types.str; };
          };
        }));
    };

    earlyInit = lib.mkOption {
      type = lib.types.lines;
      description = "Customisations to be run before the GUI is initialised.";
      default = "";
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlays.emacs ];

    programs.emacs.earlyInit = lib.mkDefault ''

    '';

    programs.emacs.init = ''
      ${requires}
    '';

    programs.emacs.extraPackages = ep: [

    ] ++ lib.attrsets.mapAttrsToList
      (name: val:
        ep.trivialBuild {
          pname = name;
          version = "x";
          src = val.src;
        })
      confPackages;

    xdg.configFile = {
      "emacs/early-init.el" = {
        text = self.lib.emacs.genPackage "early-init"
          "Config to run on startup, before the GUI is loaded."
          cfg.earlyInit;
      };
      "emacs/init.el" = {
        text = self.lib.emacs.genPackage "init"
          "Initialise emacs configuration" [ ] [ ]
          cfg.init;
      };
    };
  };
}
