# Custom Keymaps

## Default Keymaps

- `<Leader>w`：ファイルを保存
- `<Esc><Esc>`：ハイライトを消す

## Plugin Keymaps

### github

- `<Leader>ga`:そのファイルを`git add`する
- `<Leader>gc`:そのファイルを`git commit`する
- `<Leader>gl`:`git log`を出力する
- `<Leader>gd`:`diff`を表示する
- `<Leader>gs`:`git blame`を表示する

### 補完

- `<C-d>`：ドキュメントのスクロール（上）
- `<C-f>`：ドキュメントのスクロール（下）
- `<C-Space>`：補完を止める
- `<C-e>`：補完のメニューを閉じる
- `<CR>`：補完を確定する

### lsp

- `<C-j>`：diagnostic間で移動する
- `K`：ドキュメントをhoverする（？）
- `gd`：定義ジャンプ
- `<C-k>`：signatureのヘルプを開く
- `gp`：定義参照
- `gr`：まとめて名前を変更

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

- `<C-f>`：file finderを開く
- `<C-r>`：謎コマンド(`telescope.builtin.live_grep()`)
- `\\\\`：バッファを検索する
- `<C-t>`：helpの検索
- `;;`：直前にやった検索と同じものを行う
- `<C-e>`：diagnosticのfizzy finderを開く
