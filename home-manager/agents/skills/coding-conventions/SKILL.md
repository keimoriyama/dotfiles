---
name: coding-conventions
description: コード、テスト、またはコミットを作成・変更する実装作業で、設計先行、説明責任の分担、テスト方針を適用する。コードを変更しない調査や説明だけの依頼には使用しない。
---

# コーディング規約

## 振る舞い

1. コードを書き始める前に、設計をユーザーへ提示する。
2. 書き始める前に `references/responsibilities.md` を読み、成果物ごとに何を説明するかを確認する。
3. テストを書くときは `references/testing.md` を読み、対象と範囲を決める。
4. テストは pre-commit から実行する。変更のたびに直接実行しない。

## 知識

- `references/responsibilities.md` — コード・テスト・コミットログ・コメントの説明責任の分担。
- `references/testing.md` — テストを書く対象、最低限のケース、実行方法。
