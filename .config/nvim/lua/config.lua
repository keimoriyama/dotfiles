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
vim.g.clipboard = {
  name = 'tmux',
  copy = {
    ["+"] = { 'tmux', 'load-buffer', '-w', '-' },
    ["*"] = { 'tmux', 'load-buffer', '-w', '-' },
  },
  paste = {
    ["+"] = { 'tmux', 'save-buffer', '-' },
    ["*"] = { 'tmux', 'save-buffer', '-' },
  },
  cache_enabled = false,
}
vim.opt.scrolloff = 0
if vim.fn.has("mac") == 1 then
	vim.cmd([[set clipboard+=unnamed]])
else
	vim.cmd([[set clipboard^=unnamedplus]])
end

local notify                    = vim.notify
vim.notify                      = function(msg, ...)
	if msg:match("warning: multiple different client offset_encodings") then
		return
	end
	notify(msg, ...)
end
vim.g.loaded_man = 1
vim.g.did_install_default_menus = 1
vim.g.did_install_syntax_menu   = 1
vim.g.did_indent_on             = 1
vim.g.did_load_filetypes        = 1
vim.g.did_load_ftplugin         = 1
vim.g.loaded_2html_plugin       = 1
vim.g.loaded_gzip               = 1
vim.g.loaded_man                = 1
vim.g.loaded_matchit            = 1
vim.g.loaded_tar = 1
vim.g.loaded_matchparen         = 1
vim.g.loaded_netrwPlugin        = 1
vim.g.loaded_remote_plugins     = 1
vim.g.loaded_shada_plugin       = 1
vim.g.loaded_spellfile_plugin   = 1
vim.g.loaded_tarPlugin          = 1
vim.g.loaded_tutor_mode_plugin  = 1
vim.g.loaded_zip = 1
vim.g.loaded_zipPlugin          = 1
vim.g.skip_loading_mswin        = 1
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_netrwSettings = 1
vim.g.loaded_netrwFileHandlers = 1
vim.g.loaded_getscript = 1
vim.g.loaded_getscriptPlugin = 1

-- vim.g.python3_host_prog = '/opt/homebrew/bin/python3'
vim.opt.mouse                   = ""
