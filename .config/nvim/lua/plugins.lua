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

if vim.g.vscode then return nil end
lazy.setup({
	-- color scheme
	'folke/tokyonight.nvim',
	-- status line
	'nvim-lualine/lualine.nvim',

	-- indent
	"lukas-reineke/indent-blankline.nvim",

	-- auto pair and tag close
	'windwp/nvim-autopairs',
	'windwp/nvim-ts-autotag',

	-- surround
	"kylechui/nvim-surround",

	-- git
	'dinhhuy258/git.nvim',
	'lewis6991/gitsigns.nvim',
	'airblade/vim-gitgutter',
	'akinsho/git-conflict.nvim',

	-- auto comment out
	'numToStr/Comment.nvim',

	'luochen1990/rainbow',

	-- dot repeat
	'tpope/vim-repeat',

	-- linter, formatter
	'jose-elias-alvarez/null-ls.nvim',

	-- utility
	'nvim-lua/plenary.nvim',
	'nvim-lua/popup.nvim',

	-- fuzzy finder
	'nvim-telescope/telescope.nvim',
	{
		'nvim-telescope/telescope-file-browser.nvim',
		dependencies = { 
			"nvim-telescope/telescope.nvim",
			"nvim-lua/plenary.nvim" 
		}
	},

	-- File icons
	'kyazdani42/nvim-web-devicons',

	-- treesitter
	{ 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
	{ "yioneko/nvim-yati",               dependencies = "nvim-treesitter/nvim-treesitter" },

	-- bufferline
	'akinsho/nvim-bufferline.lua',

	-- show color
	'norcalli/nvim-colorizer.lua',

	-- csv
	'Decodetalkers/csv-tools.lua',

	-- comment
	'folke/todo-comments.nvim',

	-- views
	'petertriho/nvim-scrollbar',

	-- keep lastest cursor position
	'ethanholz/nvim-lastplace',

	-- sidebar-nvim
	'sidebar-nvim/sidebar.nvim',

	-- mkdir
	'jghauser/mkdir.nvim',

	---
	"tversteeg/registers.nvim",

	--- code action list
	"aznhe21/actions-preview.nvim",

	--- lsp
	"neovim/nvim-lspconfig",
	'williamboman/mason.nvim',
	'williamboman/mason-lspconfig.nvim',

	--- cmp
	'onsails/lspkind-nvim', -- vscode-like pictograms
	'L3MON4D3/LuaSnip',
	'hrsh7th/cmp-nvim-lsp',
	'hrsh7th/cmp-buffer',
	'hrsh7th/cmp-path',
	'hrsh7th/nvim-cmp',
	'hrsh7th/cmp-nvim-lsp-signature-help',
	'yutkat/cmp-mocword',
	'hrsh7th/cmp-cmdline',
	'ray-x/cmp-treesitter',

	--- noice
	'folke/noice.nvim',
	'MunifTanjim/nui.nvim',
	'folke/trouble.nvim',
	'rcarriga/nvim-notify',

	-- template
	'mattn/vim-sonictemplate',
	-- markdown
	{
		"iamcco/markdown-preview.nvim",
		build = "cd app && npm install",
		init = function() vim.g.mkdp_filetypes = { "markdown" } end,
		ft = { "markdown" }
	},
	'mattn/vim-maketable',
	--- obsidian
	'epwalsh/obsidian.nvim',
	'BurntSushi/ripgrep',
	-- mark visualization
	'chentoast/marks.nvim',

	-- copilot
	"zbirenbaum/copilot.lua",
})

vim.cmd [[colorscheme tokyonight]]
