-- エンコーディングの指定
vim.opt.fileencoding = "utf-8"
-- indexを相対表記にする
vim.opt.relativenumber = true
-- リアルタイム検索にする
vim.opt.hlsearch = true
vim.opt.incsearch = true
-- 最下ウィンドウにいつステータス行を表示するか
vim.opt.laststatus = 2
-- コマンドライン補完の有効化
vim.opt.wildmenu = true
-- 直前の行から新しいインデントを指定する
vim.opt.autoindent = true
-- vim以外でファイルを編集した時
vim.opt.autoread = true
-- バックアップファイルを作成しない
vim.b.noswapfile = true
vim.b.nobackup = true
vim.b.nowritebackup = true
-- カレントバッファの強調表示
vim.opt.syntax = "enable"
-- tabの表示幅の修正
vim.opt.tabstop = 4
-- インデントに使うshift幅
vim.opt.shiftwidth = 4
-- ファイルの変更評価
vim.opt.modifiable = true
-- コピペの共通化
vim.opt.clipboard:append({ unnamedplus = true })
-- スワップファイルを作らない
vim.opt.updatetime = 200

vim.opt.pumblend = 30

if vim.fn.has("mac") == 1 then
	vim.cmd([[set clipboard+=unnamed]])
else
	vim.cmd([[set clipboard^=unnamedplus]])
end
-- シェルの設定
vim.g.shell = "/opt/homebrew/bin/fish"

vim.g.mapleader = " "
