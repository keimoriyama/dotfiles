local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

vim.g.mapleader = " "

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
if vim.fn.has("mac") == 1 then
	vim.cmd([[set clipboard+=unnamed]])
else
	vim.cmd([[set clipboard^=unnamedplus]])
end

-- ESC*2 でハイライトやめる
keymap("n", "<Esc><Esc>", ":<C-u>set nohlsearch<Return>", opts)
keymap("n", "<Leader>bd", ":bd<CR>", opts)
keymap("n", "<Leader>w", ":w<CR>", opts)
keymap("n", "<Leader>q", ":wq<CR>", opts)
keymap("n", "<Leader>Q", ":q!<CR>", opts)
keymap("n", "+", "<C-a>", opts)
keymap("n", "-", "<C-x>", opts)
keymap("n", "n", "nzz", opts)
keymap("n", "N", "Nzz", opts)
keymap("n", "*", "*zz", opts)
keymap("n", "g*", "g*zz", opts)
keymap("n", "g#", "g#zz", opts)

vim.api.nvim_set_keymap("t", "<C-h>", "<cmd>wincmd h<CR>", opts)
vim.api.nvim_set_keymap("t", "<C-l>", "<cmd>wincmd l<CR>", opts)
vim.api.nvim_set_keymap("t", "<C-j>", "<cmd>wincmd j<CR>", opts)
vim.api.nvim_set_keymap("t", "<C-k>", "<cmd>wincmd k<CR>", opts)
vim.api.nvim_set_keymap("n", "<C-h>", "<C-w>h", opts)
vim.api.nvim_set_keymap("n", "<C-k>", "<C-w>k", opts)
vim.api.nvim_set_keymap("n", "<C-j>", "<C-w>j", opts)
vim.api.nvim_set_keymap("n", "<C-l>", "<C-w>l", opts)
