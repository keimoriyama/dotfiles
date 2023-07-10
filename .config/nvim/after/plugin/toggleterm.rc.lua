local status, toggleterm = pcall(require, "toggleterm")

if not status then
	return
end

toggleterm.setup()

local option = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<C-t>", "<cmd>ToggleTerm<CR>", option)
vim.api.nvim_set_keymap("t", "<C-t>", "<cmd>ToggleTerm<CR>", option)
