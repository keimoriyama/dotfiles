# dotfiles

## macOS (nix-darwin)

### Configurations

ユーザー名ごとに darwin 構成を用意しています。ホストモジュール (`hosts/darwin/`) は共通で、`username` だけが異なります。

| 構成 | ユーザー名 | 用途 |
| --- | --- | --- |
| `my-config` | `kei` | 個人用 (デフォルト) |
| `work-config` | `kei.moriyama` | 仕事用 |

`nix run .#update` は構成名を引数に取ります (省略時は `my-config`)。

```sh
nix run .#update              # my-config (kei)
nix run .#update work-config  # work-config (kei.moriyama)
```

### Install

```sh
# Clone the repository
cd ~
git clone <your-repo-url> dotfiles
cd dotfiles

# Install Determinate Nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Apply configuration (仕事用マシンなら `nix run .#update work-config`)
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

`nix run` を使わず直接適用する場合:

```sh
sudo darwin-rebuild switch --flake ~/dotfiles#work-config
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
sudo nixos-rebuild switch --flake ~/dotfiles#my-config
```

### Update

```sh
cd ~/dotfiles

# Update flake inputs
nix flake update

# Apply the configuration
sudo nixos-rebuild switch --flake .#my-config
```

個別 input だけ更新したい場合:

```sh
cd ~/dotfiles
nix flake lock --update-input nixpkgs
sudo nixos-rebuild switch --flake .#my-config
```
