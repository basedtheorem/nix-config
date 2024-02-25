<h2 align="center">1rns/nome</h1>


<p align="center">
    <img alt="Linux" src="https://img.shields.io/badge/Linux-000000?style=for-the-badge&logo=Linux&logoColor=white">
    <img alt="NixOS" src="https://img.shields.io/badge/Nixos-FFFFFF?style=for-the-badge&logo=NixOS&logoColor=black">
</p>

#### Directory structure

- [flake](flake): Entry point.
- [lib](lib): Utilities used throughout the flake.
- [packages](packages): Package definitions.
- [users](users): Home-manager configurations.
  - [users/modules](hosts/modules) HM modules (profiles, and presets).
- [hosts](hosts): NixOS configurations.
  - [hosts/modules](hosts/modules)
- [upgrade.fish](upgrade): Upgrade script (update lock, `nixos-rebuild`, `home-manager switch`, gc).


#### References

- [NobbZ/nixos-config](https://github.com/NobbZ/nixos-config)
- [ViperML/dotfiles](https://github.com/viperML/dotfiles)
- [adisbladis/nixconfig](https://github.com/adisbladis/nixconfig)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
- [zimbatm/home](https://github.com/zimbatm/home)
