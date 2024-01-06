# ?

#### Useful snippets

```nix

# For use with nix repl after loading the flake.
parts.lib.mkFlake { ... }: {
  debug = true;
  /* ... */
}

# Can be used like a print statement for debugging,
# it will print the first arg then return the second.
lib.trace (<expression>) (<expression>);

# Delete all previous generations
sudo nix-collect-garbage -d
home-manager expire-generations '-1 second'

# Update everything
cd ~/dots; git add . && git commit -m "chore: update lock" && \
  sudo nix-channel --update && nix flake update ~/dots/. && \
  sudo nixos-rebuild boot --flake ~/dots#xps && \
  home-manager switch --flake ~/dots#l &&  \
  git push origin dev && sudo nix-collect-garbage && \
reboot
```

#### References

[noogle.dev](https://noogle.dev)
[NixOS Search](https://search.nixos.org/packages?channel=unstable)
[Zero to Nix](https://zero-to-nix.com)

#### TODO

```
Name: leap-vscode-extensions
Id: front-end-captain.leap-vscode-extensions
Description: leap vscode extensions
Version: 0.0.4
Publisher: front-end-captain
VS Marketplace Link: https://marketplace.visualstudio.com/items?itemName=front-end-captain.leap-vscode-extensions
```

#### Inherit

```
# This line:
inherit (a) x y;
# is equivalent to:
x = a.x; y = a.y;
```

```
This snippet:
let
  inherit ({ x = 1; y = 2; }) x y;
in [ x y ]
# is equivalent to:
let
  x = { x = 1; y = 2; }.x;
  y = { x = 1; y = 2; }.y;
in [ x y ]

```bash
ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts
# github.com:22 SSH-2.0-babeld-8e18a363
```
