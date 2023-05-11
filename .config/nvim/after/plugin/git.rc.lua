local status, git = pcall(require, "git")
if not status then
	return
end

vim.g.mapleader = " "

git.setup({
	keymaps = {
		-- Open blame window
		blame = "<Leader>gb",
		-- Close blame window
		quit_blame = "q",
		-- Open blame commit
		blame_commit = "<CR>",
		-- Open file/folder in git repository
		browse = "<Leader>go",
	},
})

local option = { noremap = true, silent = true }

-- local filename = vim.fn.expand("%:p")
local file = vim.api.nvim_buf_get_name(0)
local command_add = string.format("<cmd>Git add %s<CR>", file)

vim.api.nvim_create_autocmd("BufEnter", {
	pattern = "*",
	nested = true,
	callback = function(args)
		file = vim.api.nvim_buf_get_name(0)
		command_add = string.format("<cmd>Git add %s<CR>", file)
	end,
})

vim.api.nvim_set_keymap("n", "<Leader>gc", "<cmd>Git commit<CR>", option)
vim.api.nvim_set_keymap("n", "<Leader>ga", command_add, option)
vim.api.nvim_set_keymap("n", "<Leader>gs", "<cmd>Git status<CR>", option)
vim.api.nvim_set_keymap("n", "<Leader>gp", "<cmd>Git push<CR>", option)
vim.api.nvim_set_keymap("n", "<Leader>gP", "<cmd>Git pull<CR>", option)
