local opts = { noremap = true, silent = true }
local function exit_buffer() 
	if vim.api.nvim_buf_get_name(0) == "" then
		vim.api.nvim_command("q")
	else
		vim.api.nvim_command("wq")
	end
end
vim.api.nvim_set_keymap("n", "<Esc><Esc>", ":<C-u>set nohlsearch<Return>", opts)
vim.api.nvim_set_keymap("n", "<Leader>bd", ":bd<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>w", ":w<CR>", opts)
vim.keymap.set("n", "<Leader>q", function() exit_buffer() end, opts)
vim.api.nvim_set_keymap("n", "<Leader>Q", ":q!<CR>", opts)
vim.api.nvim_set_keymap("n", "+", "<C-a>", opts)
vim.api.nvim_set_keymap("n", "-", "<C-x>", opts)
vim.api.nvim_set_keymap("n", "n", "nzz", opts)
vim.api.nvim_set_keymap("n", "N", "Nzz", opts)
vim.api.nvim_set_keymap("n", "*", "*zz", opts)
vim.api.nvim_set_keymap("n", "g*", "g*zz", opts)
vim.api.nvim_set_keymap("n", "g#", "g#zz", opts)

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
