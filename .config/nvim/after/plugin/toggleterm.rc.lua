local status, toggleterm = pcall(require, "toggleterm")
if not status then
	return
end

toggleterm.setup({
	open_mapping = [[<C-t>]],
	direction = "vertical",
	size = vim.o.columns * 0.4,
})

local option = { noremap = true, silent = true }

vim.api.nvim_set_keymap("t", "<C-h>", "<cmd>wincmd h<CR>", option)
vim.api.nvim_set_keymap("t", "<C-l>", "<cmd>wincmd l<CR>", option)
vim.api.nvim_set_keymap("t", "<C-j>", "<cmd>wincmd j<CR>", option)
vim.api.nvim_set_keymap("t", "<C-k>", "<cmd>wincmd k<CR>", option)
vim.api.nvim_set_keymap("n", "<C-h>", "<C-w>h", option)
vim.api.nvim_set_keymap("n", "<C-k>", "<C-w>k", option)
vim.api.nvim_set_keymap("n", "<C-j>", "<C-w>j", option)
vim.api.nvim_set_keymap("n", "<C-l>", "<C-w>l", option)
