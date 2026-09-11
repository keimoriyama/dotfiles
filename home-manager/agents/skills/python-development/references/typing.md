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

## 入力検証

APIレスポンスや設定ファイルなど、外部からの入力は Pydantic で検証する。
