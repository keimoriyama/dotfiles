---
name: python-development
description: Pythonコード、Pythonパッケージ、Pythonテストを作成・変更するときに、ユーザー指定のツールチェーン、型付け、入力検証規約を適用する。Pythonを扱わない作業には使用しない。
---

# Python 開発規約

## 振る舞い

1. コマンドを実行する前に `references/toolchain.md` を読み、用途に対応するツールを選ぶ。
   `pip` と `python` は直接呼ばない。
2. 型注釈を書くときは `references/typing.md` を読み、対象と記法を確認する。
3. 外部から入ってくる値を扱うコードを書くときは `references/typing.md` の検証規約に従う。
4. テストと型チェックは pre-commit からだけ実行する。直接叩かない。

## 知識

- `references/toolchain.md` — 用途ごとに使うツールと呼び出し方。
- `references/typing.md` — 型ヒント、データ型の表現、入力検証。
