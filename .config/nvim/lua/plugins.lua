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
	{ "ellisonleao/gruvbox.nvim" },
	{ "typicode/bg.nvim",        lazy = false },
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
		"akinsho/git-conflict.nvim",
		version = "*",
		config = function()
			require("rc.plugins.git_conflict").setup()
		end,
	},
	"airblade/vim-gitgutter",
	{
		'APZelos/blamer.nvim',
		config = function()
			vim.g.blamer_enabled = 1
			vim.g.blamer_delay = 200
		end,
	},
	-- auto comment out
	{
		"numToStr/Comment.nvim",
		config = function()
			require("rc.plugins.comment").setup()
		end,
	},
	{
		"luochen1990/rainbow",
		config = function()
			vim.g.rainbow_active = 1
		end,
	},               --
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
	{ "yioneko/nvim-yati",   dependencies = "nvim-treesitter/nvim-treesitter" },
	-- bufferline
	{
		"akinsho/nvim-bufferline.lua",
		config = function()
			require("rc.plugins.bufferline").setup()
		end,
	}, --
	-- buffer sizer
	"simeji/winresizer",
	-- show diff from recover file
	"chrisbra/Recover.vim",
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
	-- mason
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
	{
		"mattn/vim-sonictemplate",
		cmd = "Template",
		config =
			function()
				vim.g.sonictemplate_vim_template_dir = "./dotfiles/template"
				vim.g.sonictemplate_key = 0
				vim.g.sonictemplate_intelligent_key = 0
				vim.g.sonictemplate_postfix_key = 0
			end
	},
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
	--copilot
	{
		"github/copilot.vim",
		cmd = "Copilot enable",
		config = function()
			vim.g.copilot_no_maps = true
			vim.g.copilot_no_tab_map = true
			vim.g.copilot_enabled = false
			vim.keymap.set(
				"i",
				"<C-g>",
				'copilot#Accept("<CR>")',
				{ silent = true, expr = true, script = true, replace_keycodes = false }
			)
		end
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			require("rc.plugins.todo-comments").setup()
		end,
	},
	-- denops
	{
		"vim-denops/denops.vim",
		dependencies = {
			"matsui54/denops-signature_help",
			"matsui54/denops-popup-preview.vim",
			"lambdalisue/guise.vim",
			-- git
			{
				"lambdalisue/gin.vim",
				config = function() require("rc.plugins.gin").setup() end
			},
			{ dir = "~/Program/dps-helloworld" },
			-- skk
			{
				"vim-skk/skkeleton",
				config = function() require("rc.plugins.skkeleton").setup() end
			},
			-- ddc補完のための設定
			{
				"Shougo/pum.vim",
				config = function() require("rc.plugins.pum").setup() end
			},
			{
				"Shougo/ddc.vim",
				event = 'InsertEnter',
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
				config = function() require("rc.plugins.ddc").setup() end,
			},
			--ddu
			{
				"Shougo/ddu.vim",
				dependencies = {
					"Shougo/ddu-kind-file",
					"Shougo/ddu-ui-filer",
					"Shougo/ddu-ui-ff",
					"uga-rosa/ddu-source-lsp",
					"Shougo/ddu-source-file",
					"matsui54/ddu-source-help",
					"Shougo/ddu-source-file_rec",
					"Shougo/ddu-source-action",
					"shun/ddu-source-rg",
					"Shougo/ddu-column-filename",
					"ryota2357/ddu-column-icon_filename",
					"Shougo/ddu-filter-matcher_substring",
					"Shougo/ddu-filter-sorter_alpha",
					"yuki-yano/ddu-filter-fzf",
					"uga-rosa/ddu-filter-converter_devicon"
				},
				config = function()
					require("rc.plugins.ddu").setup()
					require("rc.plugins.ddu_filer").setup()
				end,
			},
		},
	},
	-- smart wards
	{
		"kana/vim-smartword",
		config = function()
			vim.keymap.set("n", "w", "<Plug>(smartword-w)")
			vim.keymap.set("n", "b", "<Plug>(smartword-b)")
			vim.keymap.set("n", "e", "<Plug>(smartword-e)")
		end
	}
}, opts)

vim.cmd([[colorscheme gruvbox]])
