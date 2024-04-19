local spec = {
	{ "nvim-lua/plenary.nvim" },
	{
		"ellisonleao/gruvbox.nvim",
		lazy = false,
		config = function()
			vim.cmd([[
			set background=dark
			colorscheme gruvbox
			]])
		end,
	},
	{
		"norcalli/nvim-colorizer.lua",
		config = function()
			vim.opt.termguicolors = true
			require("colorizer").setup({
				"*",
			})
		end,
	},
	{
		"ethanholz/nvim-lastplace",
		event = { "BufRead", "BufEnter" },
		config = function()
			require("nvim-lastplace").setup()
		end,
	},
	{
		"uga-rosa/ugaterm.nvim",
		config = function()
			vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>UgatermOpen -toggle<cr>", { noremap = true, silent = true })
		end,
	},
	{
		"kylechui/nvim-surround",
		config = function()
			require("nvim-surround").setup()
		end,
	},
	{
		"mattn/vim-sonictemplate"
		init = function()
vim.g.sonictemplate_key = 0
vim.g.sonictemplate_intelligent_key = 0
vim.g.sonictemplate_postfix_key = 0
vim.g.sonictemplate_vim_template_dir = "~/.dotfiles/.config/nvim/template"
		end
	}
}

return spec
