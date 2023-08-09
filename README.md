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
