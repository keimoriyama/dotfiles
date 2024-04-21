vim.g.mapleader = " "
-- エンコーディングの指定
vim.opt.fileencodings = "utf-8,iso-2022-jp,euc-jp,sjis"
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
vim.opt.cmdheight = 0
-- コピペの共通化
vim.opt.clipboard:append({ unnamedplus = true })
vim.opt.splitright = true
-- vim.opt_local.scrolloff = 9999

if vim.fn.has("mac") == 1 then
	vim.cmd([[set clipboard+=unnamed]])
else
	vim.cmd([[set clipboard^=unnamedplus]])
end

local notify = vim.notify
vim.notify = function(msg, ...)
	if msg:match("warning: multiple different client offset_encodings") then
		return
	end
	notify(msg, ...)
end
