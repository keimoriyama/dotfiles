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
