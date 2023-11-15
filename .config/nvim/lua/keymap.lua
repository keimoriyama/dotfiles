local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }


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
vim.api.nvim_set_keymap("n", "<C-m>", "<cmd>messages<cr>", opts)


vim.api.nvim_set_keymap("n", "<leader>ss", "<C-w>s", opts)
vim.api.nvim_set_keymap("n", "<leader>sv", "<C-w>v", opts)
vim.api.nvim_set_keymap("t", "<Esc>", "<C-\\><C-n>", opts)
