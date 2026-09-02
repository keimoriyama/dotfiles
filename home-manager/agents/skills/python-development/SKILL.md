---
name: python-development
description: Pythonコード、Pythonパッケージ、Pythonテストを作成・変更するときに、ユーザー指定のツールチェーン、型付け、入力検証規約を適用する。Pythonを扱わない作業には使用しない。
---

# Python 開発規約

- パッケージ管理と実行には `uv` を使う。`pip` や `python` を直接呼ばず、`uv add` または `uv run` を使う。
- フォーマット、lint、import の整理には `ruff` を使う。
- 型チェックには `ty` を使う。
- 公開する関数とメソッドの引数・戻り値には型ヒントを付ける。
- 複雑なデータ型には `dataclass` または `TypedDict` を使う。
- APIレスポンスや設定ファイルなど、外部からの入力は Pydantic で検証する。
- テストは pytest で書く。
- テストと型チェックは pre-commit からだけ実行する。
- NumPyやPyTorchなどの多次元配列には Jaxtyping を使い、次元数まで型注釈する。
