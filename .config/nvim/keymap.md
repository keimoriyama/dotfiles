# Custom Keymaps

## Default Keymaps

- `<Leader>w`：ファイルを保存

## Plugin Keymaps

### 補完

- `<C-n>` or `<Tab>`：ドキュメントのスクロール（上）
- `<C-p>` or `<S-Tab>`：ドキュメントのスクロール（下）
- `<C-l>`：補完を呼び出す
- `<C-e>`：補完のメニューを閉じる
- `<CR>`：補完を確定する

## lsp(mason)

- `<Leader>e`:diagnosticを開く
- `[d`：次のdiagnosticに移動する
- `]d`：前のdiagnosticに移動する
- `<space>q`：diagnosticをバッファで開く？
- `gD`：宣言ジャンプ
- `gd`：定義ジャンプ
- `gi`：実装ジャンプ
- `gr`：ドキュメントの表示
- `H`:カーソル下の情報を表示する
- `<Leader>D`：型定義にジャンプする
- `<Leader>rn`：カーソル下の変数名をリネームする
- `<Leader>f`：コードのフォーマット

## telescope

- `;f`：file finderを開く
- `;r`：入力した文字列を含む場所を全てのファイルから検索する
- `;b`：バッファを検索する
- `;t`：helpの検索
- `;e`：diagnosticのfizzy finderを開く
- `sf`：ディレクトリツリーを開く
- `;k`：コマンドの検索

## コメントアウト

- `col`：１行だけコメントアウト
- `cob`：ブロック全体をコメントアウト

## Gitでconflictした時のコマンド

- `co`：手元にある変更を反映する
- `ct`：リモートの変更を反映する
- `]x`：前のconflictに移動
- `[x`：次のconflictに移動

