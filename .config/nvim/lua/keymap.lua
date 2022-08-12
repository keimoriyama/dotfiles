-- エンコーディングの指定
vim.o.fileencoding='utf-8'
-- indexを相対表記にする
vim.o.relativenumber=true
-- リアルタイム検索にする
vim.o.hlsearch=true
vim.o.incsearch=true
-- 最下ウィンドウにいつステータス行を表示するか
vim.o.laststatus=2
-- コマンドライン補完の有効化
vim.o.wildmenu=true
-- 直前の行から新しいインデントを指定する
vim.o.autoindent=true
-- vim以外でファイルを編集した時
vim.o.autoread=true
-- バックアップファイルを作成しない
vim.b.noswapfile=true
vim.b.nobackup=true
vim.b.nowritebackup=true
-- カレントバッファの強調表示
vim.o.syntax='enable'
-- tabの表示幅の修正
vim.o.tabstop=4
-- インデントに使うshift幅
vim.o.shiftwidth=4
-- ファイルの変更評価
vim.o.modifiable=true
-- コピペの共通化
vim.opt.clipboard:append({ unnamedplus = true })
if vim.fn.has('mac')==1 then
  vim.cmd[[set clipboard+=unnamed]]
else
  vim.cmd[[set clipboard^=unnamedplus]]
end
-- シェルの設定
vim.g.shell='/opt/homebrew/bin/fish'

vim.g.mapleader=' '

vim.api.nvim_set_keymap('n', '<Leader>w', ':w<CR>', {silent=true})
vim.api.nvim_set_keymap('n', '<Esc><Esc>', ':nohlsearch,<CR>', {silent=true})
