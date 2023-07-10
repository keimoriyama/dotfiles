local status, ugaterm = pcall(require, "ugaterm")
if not status then
	return
end
ugaterm.setup()
local option = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<C-t>", "<cmd>UgatermToggle<CR>", option)
vim.api.nvim_set_keymap("t", "<C-t>", "<cmd>UgatermToggle<CR>", option)
