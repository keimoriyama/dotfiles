local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
-- print(lazypath)
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

local status, lazy = pcall(require, "lazy")
if not status then
	print("lazy is not installed")
	return
end

local opts = {
	defaults = {
		lazy = false,
	},
	change_detection = {
		enabled = false,
		notify = false
	},
	performance = {
		cache = {
			enabled = true,
		},
	},
}

if vim.g.vscode then
	return nil
end

-- sonictemplateのキーマップの無効化
vim.g.sonictemplate_key = 0
vim.g.sonictemplate_intelligent_key = 0
vim.g.sonictemplate_postfix_key = 0


lazy.setup("rc.plugins", opts)

vim.cmd([[colorscheme gruvbox]])
