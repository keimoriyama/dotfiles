vim.g.mapleader = " "
-- エンコーディングの指定
vim.opt.fileencodings = "utf-8,iso-2022-jp,euc-jp,sjis"
-- indexを相対表記にする
vim.opt.relativenumber = true
-- リアルタイム検索にする
vim.opt.hlsearch = true
vim.opt.incsearch = true
-- 最下ウィンドウにいつステータス行を表示するか
vim.opt.laststatus = 3
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
-- vim.opt.cmdheight = 0
-- コピペの共通化
vim.opt.clipboard:append({ unnamedplus = true })
vim.opt.splitright = true

vim.opt.scrolloff = 0
if vim.fn.has("mac") == 1 then
	vim.cmd([[set clipboard+=unnamed]])
else
	vim.cmd([[set clipboard^=unnamedplus]])
end

vim.opt.mouse = ""

-- keymaps
local opts = { noremap = true, silent = true }
local function exit_buffer()
	if vim.api.nvim_buf_get_name(0) == "" then
		vim.api.nvim_command("q")
	else
		vim.api.nvim_command("wq")
	end
end

vim.api.nvim_set_keymap("n", "<Esc><Esc>", ":<C-u>set nohlsearch<Return>", opts)
-- vim.api.nvim_set_keymap("n", "<Leader>bd", ":bd<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>w", ":w<CR>", opts)
vim.keymap.set("n", "<Leader>q", function()
	exit_buffer()
end, opts)
vim.api.nvim_set_keymap("n", "<Leader>Q", ":q!<CR>", opts)
vim.api.nvim_set_keymap("n", "+", "<C-a>", opts)
vim.api.nvim_set_keymap("n", "-", "<C-x>", opts)
vim.api.nvim_set_keymap("t", "<Esc>", [[<C-\><C-n>]], opts)
vim.api.nvim_set_keymap("n", "<C-j>", ":bnext<CR>", opts)
vim.api.nvim_set_keymap("n", "<C-k>", ":bnext<CR>", opts)
vim.api.nvim_set_keymap("v", "<Leader>cw", "g<C-G>", opts)
-- require("dpp_config")
require("lsp")
require("mini_config")
