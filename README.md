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
git clone <your-repo-url> ghq/github.com/keimoriyama/dotfiles
cd ghq/github.com/keimoriyama/dotfiles

# Install Determinate Nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Apply configuration (仕事用マシンなら `nix run .#update work-config`)
nix run .#update
```

### Update packages

`home-manager` のパッケージインストールは `nix-darwin` に統合されているため、適用は `nix-darwin` 側の更新だけで完了します。

```sh
cd ~/ghq/github.com/keimoriyama/dotfiles

# flake inputs を更新
nix flake update

# nix-darwin と home-manager の設定をまとめて適用
nix run .#update
```

個別 input だけ更新したい場合:

```sh
cd ~/ghq/github.com/keimoriyama/dotfiles
nix flake lock --update-input nixpkgs
nix run .#update
```

`nix run` を使わず直接適用する場合:

```sh
sudo darwin-rebuild switch --flake ~/ghq/github.com/keimoriyama/dotfiles#work-config
```

### Emacs パッケージの利用統計

`package-usage-mode` がコマンドの実行回数、メジャーモードの有効化回数、ロードされたセッション数をパッケージごとに記録している。
JSON のレポートを書き出し、AI エージェントに読ませて使っていないパッケージを洗い出す。

```sh
# ~/.emacs.d/package-usage-report.json に書き出す
emacsclient --eval '(package-usage-write-report)'
```

レポートは利用回数の少ない順に並び、冒頭の `notes` に各フィールドの読み方を書いてある。
エージェントには次のように依頼する。

```text
~/.emacs.d/package-usage-report.json を読み、使われていないパッケージを
home-manager/emacs/epkgs.nix と init.org から削除してください。
```

## NixOS WSL

### Initial Setup

1. Install NixOS-WSL following the [official instructions](https://github.com/nix-community/NixOS-WSL)

2. Clone this repository in WSL:
```sh
cd ~
git clone <your-repo-url> ghq/github.com/keimoriyama/dotfiles
cd ghq/github.com/keimoriyama/dotfiles
```

3. Build and activate the configuration:
```sh
sudo nixos-rebuild switch --flake ~/ghq/github.com/keimoriyama/dotfiles#my-config
```

### Update

```sh
cd ~/ghq/github.com/keimoriyama/dotfiles

# Update flake inputs
nix flake update

# Apply the configuration
sudo nixos-rebuild switch --flake .#my-config
```

個別 input だけ更新したい場合:

```sh
cd ~/ghq/github.com/keimoriyama/dotfiles
nix flake lock --update-input nixpkgs
sudo nixos-rebuild switch --flake .#my-config
```
