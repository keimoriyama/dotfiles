# dotfiles

## macOS (nix-darwin)

### Install

```sh
# Clone the repository
cd ~
git clone <your-repo-url> dotfiles
cd dotfiles

# Install Determinate Nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Apply configuration
nix run .#update
```

### Update packages

`home-manager` のパッケージインストールは `nix-darwin` に統合されているため、適用は `nix-darwin` 側の更新だけで完了します。

```sh
cd ~/dotfiles

# flake inputs を更新
nix flake update

# nix-darwin と home-manager の設定をまとめて適用
nix run .#update
```

個別 input だけ更新したい場合:

```sh
cd ~/dotfiles
nix flake lock --update-input nixpkgs
nix run .#update
```

## NixOS WSL

### Initial Setup

1. Install NixOS-WSL following the [official instructions](https://github.com/nix-community/NixOS-WSL)

2. Clone this repository in WSL:
```sh
cd ~
git clone <your-repo-url> dotfiles
cd dotfiles
```

3. Build and activate the configuration:
```sh
sudo nixos-rebuild switch --flake ~/dotfiles#nixos-wsl
```

### Update

```sh
cd ~/dotfiles

# Update flake inputs
nix flake update

# Apply the configuration
sudo nixos-rebuild switch --flake ~/dotfiles#nixos-wsl
```

個別 input だけ更新したい場合:

```sh
cd ~/dotfiles
nix flake lock --update-input nixpkgs
sudo nixos-rebuild switch --flake ~/dotfiles#nixos-wsl
```
