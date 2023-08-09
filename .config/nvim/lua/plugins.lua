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
		lazy = false,
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
-- local plugs=require('rc.plugins')

lazy.setup({
	-- color scheme
	"folke/tokyonight.nvim",
	-- status line
	{
		"nvim-lualine/lualine.nvim",
		config = function()
			require("rc.plugins.lualine").setup()
		end,
	},
	-- indent
	{
		"lukas-reineke/indent-blankline.nvim",
		config = function()
			require("rc.plugins.indent_blank").setup()
		end,
	}, --
	-- auto pair and tag close
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = function()
			require("rc.plugins.autopairs").setup()
		end,
	},
	{
		"windwp/nvim-ts-autotag",
		event = "InsertEnter",
		config = function()
			require("nvim-ts-autotag").setup()
		end,
	}, --
	-- surround
	{
		"kylechui/nvim-surround",
		config = function()
			require("nvim-surround").setup()
		end,
	}, --
	-- git
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("rc.plugins.gitsigns").setup()
		end,
	},
	{
		"dinhhuy258/git.nvim",
		config = function()
			require("rc.plugins.git").setup()
		end,
	},
	{
		"akinsho/git-conflict.nvim",
		version = "*",
		config = function()
			require("rc.plugins.git_conflict").setup()
		end,
	},
	"airblade/vim-gitgutter",
	-- auto comment out
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	},
	{
		"luochen1990/rainbow",
		config = function()
			vim.g.rainbow_active = 1
		end,
	}, --
	-- dot repeat
	"tpope/vim-repeat", --
	-- utility
	"nvim-lua/plenary.nvim",
	"nvim-lua/popup.nvim",
	-- File icons
	{
		"kyazdani42/nvim-web-devicons",
		config = function()
			require("rc.plugins.web-devicon").setup()
		end,
	},
	-- treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("rc.plugins.treesitter").setup()
		end,
	},
	{ "yioneko/nvim-yati", dependencies = "nvim-treesitter/nvim-treesitter" },
	-- bufferline
	{
		"akinsho/nvim-bufferline.lua",
		config = function()
			require("rc.plugins.bufferline").setup()
		end,
	}, --
	-- -- show color
	{
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("rc.plugins.colorizer").setup()
		end,
	}, --
	-- csv
	{
		"Decodetalkers/csv-tools.lua",
		ft = {
			"csv",
		},
		config = function()
			require("rc.plugins.csvtools").setup()
		end,
	},
	-- keep lastest cursor position
	{
		"ethanholz/nvim-lastplace",
		config = function()
			require("nvim-lastplace").setup({})
		end,
	},
	-- mkdir
	"jghauser/mkdir.nvim",
	{
		"williamboman/mason.nvim",
		dependencies = {
			-- lsp
			"neovim/nvim-lspconfig",
			"williamboman/mason-lspconfig.nvim",
			-- linter
			"mfussenegger/nvim-lint",
			-- formatter
			"mhartington/formatter.nvim",
		},
		config = function()
			-- linter,formatter,lspの設定が書いてある
			require("rc.plugins.mason").setup()
		end,
	},
	-- vscode-like pictograms
	{
		"onsails/lspkind-nvim",
		config = function()
			require("rc.plugins.lspkind").setup()
		end,
	},
	-- template
	-- "mattn/vim-sonictemplate",
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
	{
		"akinsho/toggleterm.nvim",
		keys = { { "<C-t>", ":ToggleTerm<CR>" } },
		config = function()
			require("rc.plugins.toggleterm").setup()
		end,
	},
	-- tex plugin
	{
		"lervag/vimtex",
		ft = { "tex", "bib" },
		config = function()
			require("rc.plugins.vimtex").setup()
		end,
	},
	-- snippet
	{
		"hrsh7th/vim-vsnip",
		event = { "InsertEnter" },
		config = function()
			require("rc.plugins.vsnip").setup()
		end,
	},
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
			require("rc.plugins.copilot").setup()
		end,
	},
	{
		"zbirenbaum/copilot-cmp",
		config = function()
			require("copilot_cmp").setup()
		end,
	},

	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-telescope/telescope-file-browser.nvim",
		},
		config = function()
			require("rc.plugins.telescope").setup()
		end,
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			require("rc.plugins.todo").setup()
		end,
	},
	-- { dir = "~/Program/example_plugin/" },
	-- { dir = "~/Program/nvim-lint/" },
}, opts)

vim.cmd([[colorscheme tokyonight]])
