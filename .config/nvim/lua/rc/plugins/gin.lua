local M = {}
function M.setup()
	local opts = { noremap = true, silent = true }
	vim.keymap.set("n", "<leader>gs", ":GinStatus<CR>", opts)
	vim.keymap.set("n", "<leader>ga", ":Gin add .<CR>", opts)
	vim.keymap.set("n", "<leader>gd", ":GinDiff<CR>", opts)
	vim.keymap.set("n", "<leader>gb", ":GinBranch<CR>", opts)
	vim.keymap.set("n", "<leader>gl", ":GinLog<CR>", opts)
end

return M
