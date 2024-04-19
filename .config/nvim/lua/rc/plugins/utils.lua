local spec = {
	{
		"ellisonleao/gruvbox.nvim",
		event = { "BufNewFile", "BufRead" },
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
		"nvim-lua/plenary.nvim",
	},
	{
		"ethanholz/nvim-lastplace",
		event = { "BufNewFile", "BufRead" },
		config = function()
		require("nvim-lastplace").setup()
		end
	},
	{
		'uga-rosa/ugaterm.nvim',
		config = function()
			vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>UgatermOpen -toggle<cr>", { noremap = true, silent = true })
		end
	},
	{
		"windwp/nvim-autopairs",
			event= {'InsertEnter'},
			config = function()
			require("nvim-autopairs").setup({
			disable_filetype = { "vim" },
			})
		end
	}
}

return spec
