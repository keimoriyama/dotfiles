vim.cmd[[packadd packer.nvim]]

require'packer'.startup(function()
    use'wbthomason/packer.nvim'
	-- Template plugins
	use{'mattn/vim-sonictemplate',
		setup = function()
			vim.g.sonictemplate_vim_template_dir = '~/.dotfiles/template'
		end}
	-- git plugins
	use{
		'tpope/vim-fugitive',
		setup = function()
			vim.api.nvim_set_keymap('n', '<Leader>ga', ':Gwrite<CR>',{ noremap=true, silent=true})
			vim.api.nvim_set_keymap('n', '<Leader>gc', ':Git commit<CR>',{ noremap=true, silent=true})
			vim.api.nvim_set_keymap('n', '<Leader>gl', ':Git log<CR>',{ noremap=true, silent=true})
			vim.api.nvim_set_keymap('n', '<Leader>gd', ':Git diff<CR>',{ noremap=true, silent=true})
			vim.api.nvim_set_keymap('n', '<Leader>gs', ':Git<CR>',{ noremap=true, silent=true})
			vim.api.nvim_set_keymap('n', '<Leader>gb', ':Git blame<CR>',{ noremap=true, silent=true})
		end}
	-- color scheme
	use'lifepillar/vim-solarized8'
	-- statusline
	use'itchyny/lightline.vim'
	-- plugin for tex
	use{'lervag/vimtex', opt=true, ft={'tex'},
		setup = function()
			vim.g.latex_latexmk_options = '-pdf'
		end
	}
	-- Configurations for Nvim LSP
	use 'neovim/nvim-lspconfig' 
	use"williamboman/nvim-lsp-installer"
end)

