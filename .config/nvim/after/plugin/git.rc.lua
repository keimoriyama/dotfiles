local status, git = pcall(require, "git")

if not status then
	return
end

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

vim.api.nvim_set_keymap("n", "<leader>gp", "<cmd>git push<cr>", option)
vim.api.nvim_set_keymap("n", "<leader>gp", "<cmd>git pull<cr>", option)
vim.g.gitgutter_map_keys = 0

local status, gitconflict = pcall(requre, "git-conflict")
if not status then
	return
end

gitconflict.setup()
