# 型付けと入力検証

## 型ヒント

- 公開する関数とメソッドの引数・戻り値には型ヒントを付ける。
- 複雑なデータ型には `dataclass` または `TypedDict` を使う。

## 多次元配列

NumPy や PyTorch などの多次元配列には Jaxtyping を使い、次元数まで型注釈する。

```python
from jaxtyping import Float
from torch import Tensor

def forward(x: Float[Tensor, "batch seq dim"]) -> Float[Tensor, "batch dim"]: ...
```

- 引数・戻り値・`TypedDict` のフィールドなど、配列を受け渡すすべての箇所に付ける。
  素の `Tensor` や `np.ndarray` で注釈しない。
- 次元名は軸の意味で付ける（`"task annotator label"` など）。
  同じ名前の軸は同じ長さを表すので、長さが異なりうる軸には別の名前を使う。
- dtype に合わせて `Float` / `Int` / `Bool` を使い分ける。
  NumPy 配列は `Float[np.ndarray, "task annotator"]` のように書く。
- スカラーの配列（損失値など）は `Float[Tensor, ""]` と書く。
- jaxtyping が依存に無ければ `uv add jaxtyping` で追加する。
- ruff は形状文字列を前方参照として解析し、F722 を誤検出する。
  `pyproject.toml` の `[tool.ruff.lint]` で `ignore = ["F722"]` を設定する。
- 注釈は形状の説明として書く。実行時の形状検査（`jaxtyped` と beartype の組み合わせ）は、
  求められたときだけ導入する。

## 入力検証

APIレスポンスや設定ファイルなど、外部からの入力は Pydantic で検証する。
