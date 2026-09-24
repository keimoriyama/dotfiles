# herdr-agent 設計

herdr (https://github.com/herdrdev/herdr) で動くコーディングエージェントを、
agent-shell 風のバッファで Emacs から操作する。

- 実装: `home-manager/emacs/herdr-agent.el`
- テスト: `home-manager/emacs/herdr-agent-tests.el` (pre-commit の run-tests.sh から実行)
- 組み込み: `default.nix` の `home.file`、`init.org` の `** herdr-agent`、
  hydra「Agent Shell メニュー」の herdr 欄 (`C-c A` → h / H / g / z)

## 前提 (herdr 0.9.1 で確認した事実)

- herdr が公開するのはターミナルの読み書きだけ。ACP のような構造化出力はない。
  出力はエージェント画面のスクレイプになる。
- CLI の成功時は stdout に JSON (`agent read` は生テキスト)、失敗時は stderr に
  JSON (`{"error":{"code":..,"message":..}}`) を出して exit 1。構文エラーは exit 2。
- セッション指定は `--session` フラグでは効かず、環境変数 `HERDR_SESSION` で行う。
  解決順は `--session` > `HERDR_SOCKET_PATH` > `HERDR_SESSION` > 既定。
- `agent prompt` は bracketed paste で送るため、Claude Code が「貼り付けられた内容」
  として扱い、指示を実行しない。`pane send-text` + `pane send-keys enter` なら
  通常の入力として届く。改行を含んでも send-text の段階では送信されない。
- `agent start` は起動時ダイアログ (Claude Code のフォルダ信頼確認など) で
  `agent_not_ready` を返すが、エージェント名は登録済み。
- `agent read --source recent-unwrapped --lines N` は、TUI エージェントの
  alternate screen の履歴をスクロールして取得する。working 中は
  `agent_not_idle` で拒否される。`--source visible` はスクロールしない。
- `pane.agent_status_changed` のイベント購読はペインごとに登録が必要で、
  存在しないペインが 1 つでもあると購読全体が拒否され接続が切られる。

## 構成

| 層 | 内容 |
|---|---|
| CLI 呼び出し | `herdr-agent--call`: `make-process` で非同期実行。stderr は stdout に混ぜ、exit status で成否を判定する。`herdr-agent--call-sync` は補完候補作成用の同期版 |
| 純粋関数 | JSON 解析、エラーコード抽出、画面整形 (`herdr-agent-trim-footer`)、表示名、状態の face、送信コマンド列、一覧の行、リージョン引用 |
| エージェントバッファ | `herdr-agent-mode`。上部に読み取り専用の画面、下部に `herdr> ` 入力欄。ヘッダ行に名前・種類・状態・cwd |
| 一覧 | `herdr-agent-list-mode` (tabulated-list)。RET で開く、k でキー送信、f でフォーカス、s で起動 |
| 更新 | 全体で 1 つのタイマーが `agent list` をポーリングし、各バッファの状態を更新。herdr のバッファがなくなったら止まる |

## コマンド

- `herdr-agent-list` / `herdr-agent-open` / `herdr-agent-start` / `herdr-agent-send-region`
- バッファ内: RET・`C-c C-c` 送信、`C-j` 改行、`C-c C-k` キー送信 (ダイアログ応答)、
  `C-c C-i` 中断 (esc)、`C-c C-r` 再読込、`C-c C-f` TUI でフォーカス、`C-c C-l` 一覧
- 送信前に `agent get` で状態を確認し、blocked なら送らない。
- `herdr-agent-start` はワークスペースを新規作成し、そのルートペインで `agent start` する。
  サーバーは事前に起動しておく必要がある。

## 設定

`herdr-agent-program`、`herdr-agent-session`、`herdr-agent-read-lines`、
`herdr-agent-poll-interval`、`herdr-agent-default-kind`、`herdr-agent-kinds`、
`herdr-agent-screen-functions`

## 設計判断

- 親モードは `text-mode` にしない。`text-mode-hook` の nano-modeline がモード本体の
  後に走ってヘッダ行を上書きするため。
- ソケット API を直接使わず CLI を呼ぶ。イベント購読の扱いが重い (上記) ため、
  まずはポーリングで足りるかを見る。
- 応答部分だけを抜き出す解析はしない。画面をそのまま見せ、入力欄とフッターだけ削る。

## 既知の問題

現在の実装には次の不具合が残っている。

1. **送信直後の読み込み競合**: 送信直後、herdr がまだ working を検出していない間に
   `recent-unwrapped` で読むと、履歴取得のスクロール中にエージェントが応答を始め、
   Claude Code の画面が上にスクロールしたまま (「1 new message ↓」) になる。
   以後の読み込みに新しい応答が現れない。新しい送信をすると最下部に戻る。
2. **working 中の読み込み失敗**: working 中の `recent-unwrapped` は `agent_not_idle`
   で失敗し、現状はエラーを無視するだけなので、作業中の画面が更新されない。
3. **短いターンの取りこぼし**: ポーリング間隔より短いターンでは状態の変化
   (idle→working→idle) を見逃し、画面を読み直さない。

### 対応案 (未実装)

- 状態が working / blocked の間は `--source visible` で読む (スクロールしない)。
- 読み直しの判定を status の変化ではなく `state_change_seq` の変化にする。
  短いターンも取りこぼさない。
- 状態が変わっていない idle のエージェントは読み直さない (送信直後の競合を避ける)。
- `agent_not_idle` が返ったら `visible` で読み直す。
- 上記を `herdr-agent--busy-p` / `herdr-agent--read-args` / `herdr-agent--needs-read-p`
  という純粋関数に切り出し、テストを付ける。

## 今後の候補

- ソケット API のイベント購読でポーリングを置き換える。
- blocked / done を agent-shell-attention や osascript 通知に流す。
- herdr のペインが zsh で起動し、`~/.zshrc` がないため初期設定ウィザードが
  最初の入力を食べる。herdr の設定でシェルを fish にする。
