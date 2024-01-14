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
cd ~/nome; git add . && git commit -S -m "chore: update lock" && \
  sudo nix-channel --update && nix flake update ~/nome/. && \
  sudo nixos-rebuild boot --flake ~/nome#quartz && \
  home-manager switch --flake ~/nome#l &&  \
  git push origin dev && sudo nix-collect-garbage
```

#### References

[noogle.dev](https://noogle.dev)
[NixOS Search](https://search.nixos.org/packages?channel=unstable)
[Zero to Nix](https://zero-to-nix.com)

#### TODO

- [ ] Set SDDM theme resolution to 1440p (by repackaging it).
  - [ ] Figure out how SDDM knows where the theme is located despite only including it in systemPackages.

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

