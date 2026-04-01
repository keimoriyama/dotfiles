# dotfiles

## Install

```sh
# install non determinate nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

nix run .#update
```

## Update packages

`home-manager` のパッケージインストールは `nix-darwin` に統合されているため、適用は `nix-darwin` 側の更新だけで完了します。

```sh
# flake inputs を更新
nix flake update

# nix-darwin と home-manager の設定をまとめて適用
nix run .#update
```

個別 input だけ更新したい場合:

```sh
nix flake lock --update-input nixpkgs
nix run .#update
```
