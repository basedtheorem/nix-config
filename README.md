# ?

#### Useful snippets

```nix
lib.escapeShellArg
  ```

##### Trace

  ```nix
# For use with nix repl after loading the flake.
parts.lib.mkFlake { ... }: {
  debug = true;
  /* ... */
}

# Can be used like a print statement for debugging,
# it will print the first arg then return the second.
lib.trace
(<expression>)
(<expression>);

# Or
builtins.seq (lib.debug.showVal config)
```

##### Prefetch

`nix-prefetch-url "https..."`

##### Clean house

```bash
# Delete all previous generations and clear the store.
home-manager expire-generations '-1 second'
sudo nix-collect-garbage --delete-old
nix-collect-garbage --delete-old
```

##### Finding stray gcroots

[How to get rid of unused home manager packages?](https://discourse.nixos.org/t/how-to-get-rid-of-unused-home-manager-packages/14997/5)

> `sudo -i nix-store --gc --print-roots | egrep -v '^(/nix/var|/run/current-system|/run/booted-system|/proc|{memory|{censored)'`

##### Printing in repl

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

##### etc

- `chmod u=rwx,go=r-x foo`

##### Inherit

```nix
inherit (a) x y; # equivalent to: x = a.x; y = a.y;
```

```nix
let
  inherit ({ x = 1; y = 2; }) x y;
in
[ x y ]
# ^ is equivalent to:
let
  x = { x = 1; y = 2; }.x;
  y = { x = 1; y = 2; }.y;
in [ x y ]
```

#### Module args

```nix
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

#### References

[noogle.dev](https://noogle.dev)
[NixOS Search](https://search.nixos.org/packages?channel=unstable)
[Zero to Nix](https://zero-to-nix.com)
[Kernel Modules](https://web.archive.org/web/20240129074852/https://gist.github.com/CMCDragonkai/810f78ee29c8fce916d072875f7e1751)

#### TODO

- modularize profile
  - e.g. fish init is a single script that depends on several pkgs
- nixify:
  - ulauncher (api keys)
  - gitui
- switch from kitty to wezterm for sessions
  - resize wins
- navigate cursor history obs mc vsc C-{\,,.}
