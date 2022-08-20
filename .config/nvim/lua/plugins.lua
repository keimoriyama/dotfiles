require 'packer'.startup(function()
	-- Template plugins
	use { 'mattn/vim-sonictemplate',
		setup = function()
			vim.g.sonictemplate_vim_template_dir = '~/.dotfiles/template'
		end }
	-- github
	use { 'tpope/vim-fugitive',
		setup = function()
			vim.api.nvim_set_keymap('n', '<Leader>ga', ':Gwrite<CR>', { noremap = true, silent = true })
			vim.api.nvim_set_keymap('n', '<Leader>gc', ':Git commit<CR>', { noremap = true, silent = true })
			vim.api.nvim_set_keymap('n', '<Leader>gl', ':Git log<CR>', { noremap = true, silent = true })
			vim.api.nvim_set_keymap('n', '<Leader>gd', ':Git diff<CR>', { noremap = true, silent = true })
			vim.api.nvim_set_keymap('n', '<Leader>gs', ':Git<CR>', { noremap = true, silent = true })
			vim.api.nvim_set_keymap('n', '<Leader>gb', ':Git blame<CR>', { noremap = true, silent = true })
		end
	}

	use 'lewis6991/gitsigns.nvim'
	if vim.g.vscode then return nil end
	use 'wbthomason/packer.nvim'
	-- color scheme
	use 'lifepillar/vim-solarized8'
	vim.api.nvim_command [[colorscheme solarized8_low]]
	-- status line
	use 'nvim-lualine/lualine.nvim'
	-- plugin for tex
	use { 'lervag/vimtex', opt = true, ft = { 'tex' },
		setup = function()
			vim.g.latex_latexmk_options = '-pdf'
		end
	}
	-- treesitter
	use {
		'nvim-treesitter/nvim-treesitter',
		run = function() require('nvim-treesitter.install').update({ with_sync = true }) end,
	}
	--markdwon
	use({
		"iamcco/markdown-preview.nvim",
		run = "cd app && npm install",
		setup = function() vim.g.mkdp_filetypes = { "markdown" } end,
		ft = { "markdown" }
	})

	-- configurations for Filer
	use 'BurntSushi/ripgrep'
	use { 'nvim-telescope/telescope.nvim',
		requires = { { 'nvim-lua/plenary.nvim' } }
	}
	use 'nvim-telescope/telescope-file-browser.nvim'
	use 'kyazdani42/nvim-web-devicons'
	-- configurations for buffer
	use 'akinsho/nvim-bufferline.lua'
	-- Configurations for Nvim LSP
	use 'neovim/nvim-lspconfig'
	use 'williamboman/mason.nvim'
	use 'williamboman/mason-lspconfig.nvim'


	use 'onsails/lspkind.nvim'
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/nvim-cmp'
	use 'jose-elias-alvarez/null-ls.nvim'

	use 'glepnir/lspsaga.nvim'
	-- sidebar
	use 'sidebar-nvim/sidebar.nvim'
end)
