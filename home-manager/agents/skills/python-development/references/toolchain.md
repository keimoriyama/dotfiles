# ツールチェーン

| 用途 | ツール |
| --- | --- |
| パッケージ管理 | uv (`uv add`) |
| 実行 | uv (`uv run`) |
| フォーマット | ruff |
| lint | ruff |
| import の整理 | ruff |
| 型チェック | ty |
| テスト | pytest |

`pip` と `python` は直接呼ばない。`uv add` または `uv run` を使う。

ty と pytest は pre-commit からだけ実行する。
