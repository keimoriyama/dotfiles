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
	"ellisonleao/gruvbox.nvim",
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
	"akinsho/git-conflict.nvim",
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
	"tversteeg/registers.nvim",
	-- - code action list
	"aznhe21/actions-preview.nvim",
	-- -- sp
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
	-- -- terminal setting
	"akinsho/toggleterm.nvim",
	-- -- tex plugin
	{ "lervag/vimtex", ft = { "tex", "bib" } },
	-- -- snippet
	"hrsh7th/vim-vsnip",
	-- denops
	"vim-denops/denops.vim",
	"matsui54/denops-signature_help",
	"matsui54/denops-popup-preview.vim",
	"github/copilot.vim",
	-- ddc
	"Shougo/pum.vim",
	{
		"Shougo/ddc.vim",
		dependencies = {
			"Shougo/ddc-nvim-lsp",
			"Shougo/ddc-around",
			"LumaKernel/ddc-file",
			"matsui54/ddc-buffer",
			"Shougo/ddc-source-cmdline",
			"Shougo/ddc-source-cmdline-history",
			"Shougo/ddc-source-nvim-lsp",
			"Shougo/ddc-source-copilot",
			"uga-rosa/ddc-source-nvim-lua",
			"uga-rosa/ddc-source-vsnip",
			"LumaKernel/ddc-source-file",
			"Shougo/ddc-sorter_rank",
			"Shougo/ddc-ui-pum",
			"Shougo/ddc-converter_remove_overlap",
			"tani/ddc-fuzzy",
			"Shougo/ddc-matcher_head",
			"tani/ddc-path",
			"uga-rosa/ddc-nvim-lsp-setup",
			"Shougo/ddc-filter-converter_truncate_abbr",
		},
	},
	--ddu
	{
		"Shougo/ddu.vim",
		dependencies = {
			"Shougo/ddu-kind-file",
			"Shougo/ddu-ui-filer",
			"Shougo/ddu-ui-ff",
			-- "uga-rosa/ddu-source-lsp",
			"Shougo/ddu-source-file",
			"matsui54/ddu-source-help",
			"Shougo/ddu-source-file_rec",
			"Shougo/ddu-source-action",
			"shun/ddu-source-rg",
			"Shougo/ddu-column-filename",
			"ryota2357/ddu-column-icon_filename",
			"Shougo/ddu-filter-matcher_substring",
			"Shougo/ddu-filter-sorter_alpha",
			"uga-rosa/ddu-filter-converter_devicon",
			"yuki-yano/ddu-filter-fzf",
		},
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	-- { dir = "~/Program/example_plugin/" },
})

vim.cmd([[colorscheme tokyonight]])
