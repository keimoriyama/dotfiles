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

vim.api.nvim_set_keymap("n", "<Leader>gp", "<cmd>Git push<CR>", option)
vim.api.nvim_set_keymap("n", "<Leader>gP", "<cmd>Git pull<CR>", option)
vim.g.gitgutter_map_keys = 0
