local mini_path = vim.fn.stdpath("data") .. "/site/pack/deps/start/mini.nvim"
if vim.loop.fs_stat(mini_path) == nil then
	vim.cmd('echo "Installing `mini.nvim`" | redraw')
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/nvim-mini/mini.nvim",
		mini_path,
	})
	vim.cmd("packadd mini.nvim | helptags ALL")
end

local ok, MiniDeps = pcall(require, "mini.deps")
if not ok then
	vim.notify("mini.deps is not installed", vim.log.levels.ERROR)
	return
end

MiniDeps.setup({})

local add = MiniDeps.add

-- Pre-load settings for plugins that read globals on startup.
-- vim.g.copilot_no_maps = true
-- vim.g.copilot_hide_during_completion = 0
-- vim.g.copilot_filetypes = {
-- 	gitcommit = true,
-- 	markdown = true,
-- 	python = true,
-- 	text = true,
-- 	typescript = true,
-- 	vim = true,
-- 	yaml = true,
-- }

vim.g.sonictemplate_key = 0
vim.g.sonictemplate_intelligent_key = 0
vim.g.sonictemplate_postfix_key = 0
vim.g.sonictemplate_vim_template_dir = "~/dotfiles/home-manager/programs/neovim/nvim/template/"

vim.g.gf_improved_no_mappings = 1

vim.g.typst_pdf_viewer = "tdf"

local plugin_specs = {
	"vim-denops/denops.vim",
	"catppuccin/nvim",
	"tpope/vim-repeat",
	"ethanholz/nvim-lastplace",
	"shellRaining/hlchunk.nvim",
	"mattn/vim-sonictemplate",
	{
		source = "nvim-treesitter/nvim-treesitter",
		checkout = "main",
		hooks = {
			post_checkout = function()
				vim.cmd("TSUpdate")
			end,
		},
	},
	{ source = "nvim-treesitter/nvim-treesitter-context", depends = { "nvim-treesitter/nvim-treesitter" } },
	"kana/vim-smartword",
	"lambdalisue/gin.vim",
	"chrisbra/Recover.vim",
	"akinsho/toggleterm.nvim",
	"tani/dmacro.nvim",
	{ source = "andersevenrud/nvim_context_vt", depends = { "nvim-treesitter/nvim-treesitter" } },
	"lambdalisue/vim-file-protocol",
	"lambdalisue/vim-gf-improved",
	"skanehira/denops-silicon.vim",
	"b0o/incline.nvim",
	"monaqa/dial.nvim",
	"neovim/nvim-lspconfig",
	-- { source = "nvimtools/none-ls.nvim", depends = { "neovim/nvim-lspconfig" } },
	{ source = "vim-skk/skkeleton", depends = { "vim-denops/denops.vim" } },
	{ source = "keimoriyama/skkeleton-azik-kanatable", depends = { "vim-skk/skkeleton" } },
	"skk-dev/dict",
	{ source = "delphinus/skkeleton_indicator.nvim", depends = { "vim-skk/skkeleton" } },
	"stevearc/conform.nvim",
	"arto-app/arto.vim",
}

for _, spec in ipairs(plugin_specs) do
	add(spec)
end

require("plugins.mini")
require("plugins.legacy")

require("plugins.lsp.nvim_lspconfig")
require("plugins.lsp.conform")
require("plugins.git.gin")
require("plugins.lang.treesitter")
require("plugins.ui.skkeleton")
-- require("plugins.tools.csv_tools")
