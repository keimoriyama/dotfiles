local status, ugaterm = pcall(require, "ugaterm")
if not status then
	return
end
ugaterm.setup()
local option = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<C-t>", "<cmd>UgatermToggle<CR>", option)
vim.api.nvim_set_keymap("t", "<C-t>", "<cmd>UgatermToggle<CR>", option)

vim.api.nvim_set_keymap("t", "<C-h>", "<cmd>wincmd h<CR>", option)
vim.api.nvim_set_keymap("t", "<C-l>", "<cmd>wincmd l<CR>", option)
vim.api.nvim_set_keymap("t", "<C-j>", "<cmd>wincmd j<CR>", option)
vim.api.nvim_set_keymap("t", "<C-k>", "<cmd>wincmd k<CR>", option)
vim.api.nvim_set_keymap("n", "<C-h>", "<C-w>h", option)
vim.api.nvim_set_keymap("n", "<C-k>", "<C-w>k", option)
vim.api.nvim_set_keymap("n", "<C-j>", "<C-w>j", option)
vim.api.nvim_set_keymap("n", "<C-l>", "<C-w>l", option)
