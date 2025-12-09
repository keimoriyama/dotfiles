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

vim.api.nvim_set_keymap("v", "<Leader>cw", "g<C-G>", opts)

vim.api.nvim_set_keymap("n", "<C-x>2", "<C-w>s", opts)
vim.api.nvim_set_keymap("n", "<C-x>3", "<C-w>v", opts)
vim.api.nvim_set_keymap("t", "<Esc>", "<C-\\><C-n>", opts)
-- })
