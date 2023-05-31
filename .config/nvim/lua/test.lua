local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
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

-- LuaFormatter off
if vim.g.vscode then
	return nil
end
lazy.setup({
	-- color scheme
	"folke/tokyonight.nvim",
	-- code2image
	-- "vim-denops/denops-helloworld.vim",
	"vim-denops/denops.vim",
	-- {
	-- 	"skanehira/denops-silicon.vim",
	-- 	dependencies = { "vim-denops/denops.vim" },
	-- },
})
-- LuaFOrmatter on

vim.cmd([[colorscheme tokyonight]])
