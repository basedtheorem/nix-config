# ?

#### Useful snippets

##### Trace

```nix
# For use with nix repl after loading the flake.
parts.lib.mkFlake { ... }: {
  debug = true;
  /* ... */
}

# Can be used like a print statement for debugging,
# it will print the first arg then return the second.
lib.trace (<expression>) (<expression>);
```

##### Garbage collection

```
# Delete all previous generations
sudo nix-collect-garbage -d
home-manager expire-generations '-1 second'

# Update everything
cd ~/nome; git add . && git commit -S -m "chore: update lock" && \
  sudo nix-channel --update && nix flake update ~/nome/. && \
  sudo nixos-rebuild boot --flake ~/nome#quartz && \
  home-manager switch --flake ~/nome#l &&  \
  git push origin dev && sudo nix-collect-garbage
```

##### Print in repl

```nix
nix-repl> output = { b = true; }
nix-repl> :p output
{ b = true; }
```

##### Eval

```nix
> nix eval .#homeConfigurations.l.config.home.homeDirectory
/home/l
```

##### `self`

```nix
{
  outputs = { self }: {
    a = 1;
    b = self.a + 1;
  };
}

```

##### Inherit

```
# This line:
inherit (a) x y;
# is equivalent to:
x = a.x; y = a.y;
```

```
# This snippet:
let
  inherit ({ x = 1; y = 2; }) x y;
in
  [ x y ]
# is equivalent to:
let
  x = { x = 1; y = 2; }.x;
  y = { x = 1; y = 2; }.y;
in [ x y ]
```

#### Module args

```
// mkFlake args:
[ "config"
"flake-parts-lib"
"inputs"
"lib"
"moduleLocation"
"options"
"self"
"specialArgs" ]


// mkflake imports args:
[ "config"
"flake-parts-lib"
"inputs"
"lib"
"moduleLocation"
"nixpkgs"
"options"
"self"
"specialArgs" ]
```

##### .desktop files

> When packages are included in environment.systemPackages, a nixos module for creating the system will look for <pkg>/share/applications/*.desktop paths, and add them to this directory.

Specifically: [nixpkgs/menus.nix](https://github.com/NixOS/nixpkgs/blob/7b2f9d4732d36d305d515f20c5caf7fe1961df80/nixos/modules/config/xdg/menus.nix)

#### Style

- Permit omitting the module arguments
- Permit omitting the imports, options, or config attributes
- Avoid eliding the config attribute
  - I.e. if you define any options, always nest them underneath the config attribute.

#### References

[noogle.dev](https://noogle.dev)
[NixOS Search](https://search.nixos.org/packages?channel=unstable)
[Zero to Nix](https://zero-to-nix.com)

#### TODO

- [ ] Set SDDM theme resolution to 1440p (by repackaging it).
  - [ ] Figure out how SDDM knows where the theme is located despite only including it in systemPackages.
- [ ] Keybinds
  - [x] ctrl down moves view down
  - [x] ctrl shift down dupes cursor
  - [x] alt shift down dupes lines
  - [x] ctrl shift left selects word
  - [x] alt n dupe matches
  - [ ] ctrl shift pgdown null in micro, selects eof obs vsc
  - [ ] shift pgdown null in micro, selects pgdown obs vscode
  - [ ] navigate cursor history obs mc vsc
  - [ ] ctrl alt down null in all
  - [ ] sort uhk delete key
