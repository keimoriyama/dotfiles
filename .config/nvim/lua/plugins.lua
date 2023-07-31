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

local opts = {
	defaults = {
		lazy = true,
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
lazy.setup({
	-- color scheme
	"folke/tokyonight.nvim",
	-- -- status line
	"nvim-lualine/lualine.nvim",
	-- -- indent
	"lukas-reineke/indent-blankline.nvim", --
	-- -- auto pair and tag close
	"windwp/nvim-autopairs",
	"windwp/nvim-ts-autotag", --
	-- -- surround
	"kylechui/nvim-surround", --
	-- -- git
	"lewis6991/gitsigns.nvim",
	"dinhhuy258/git.nvim",
	{ "akinsho/git-conflict.nvim", version = "*", config = true },
	"airblade/vim-gitgutter",
	-- -- auto comment out
	"numToStr/Comment.nvim",
	"luochen1990/rainbow", --
	-- -- dot repeat
	"tpope/vim-repeat", --
	-- -- linter, formatter
	"jose-elias-alvarez/null-ls.nvim", --
	-- utility
	"nvim-lua/plenary.nvim",
	"nvim-lua/popup.nvim",
	-- -- File icons
	"kyazdani42/nvim-web-devicons",
	-- -- treesitter
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
	{ "yioneko/nvim-yati", dependencies = "nvim-treesitter/nvim-treesitter" },
	--	-- bufferline
	"akinsho/nvim-bufferline.lua", --
	-- -- show color
	"norcalli/nvim-colorizer.lua", --
	-- -- csv
	"Decodetalkers/csv-tools.lua",
	-- -- keep lastest cursor position
	"ethanholz/nvim-lastplace",
	-- -- mkdir
	"jghauser/mkdir.nvim",
	-- lsp
	"neovim/nvim-lspconfig",
	"williamboman/mason.nvim",
	"williamboman/mason-lspconfig.nvim",
	"nvimdev/lspsaga.nvim",
	-- -- vscode-like pictograms
	"onsails/lspkind-nvim",
	-- template
	"mattn/vim-sonictemplate",
	-- -- markdown
	{
		"iamcco/markdown-preview.nvim",
		build = "cd app && npm install",
		init = function()
			vim.g.mkdp_filetypes = { "markdown" }
		end,
		ft = { "markdown" },
	},
	{ "mattn/vim-maketable", ft = { "markdown" } },
	"BurntSushi/ripgrep",
	-- terminal setting
	"akinsho/toggleterm.nvim",
	-- tex plugin
	{ "lervag/vimtex", ft = { "tex", "bib" } },
	-- snippet
	"hrsh7th/vim-vsnip",
	-- cmp
	{
		"hrsh7th/nvim-cmp",
		event = { "InsertEnter", "CmdlineEnter" },
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-nvim-lsp-signature-help",
			"hrsh7th/cmp-cmdline",
			"ray-x/cmp-treesitter",
			"zbirenbaum/copilot-cmp",
			"hrsh7th/cmp-vsnip",
		},
		config = function()
			require("rc.plugins.cmp").cmp_setup()
		end,
	},
	--copilot
	{
		"zbirenbaum/copilot.lua",
		build = ":Copilot auth",
		config = function()
			require("copilot").setup({})
		end,
	},
	"nvim-telescope/telescope.nvim",
	"nvim-telescope/telescope-file-browser.nvim",
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	{ dir = "~/Program/example_plugin/" },
	"kosayoda/nvim-lightbulb",
}, opts)

vim.cmd([[colorscheme tokyonight]])

require("nvim-lightbulb").setup({
	autocmd = { enabled = true },
})
