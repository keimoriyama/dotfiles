local spec = {
	-- auto pair and tag close
	{
		"windwp/nvim-ts-autotag",
		event = "InsertEnter",
	},
	-- surround
	{
		"kylechui/nvim-surround",
	},
	{
		'APZelos/blamer.nvim',
		config = function()
			vim.g.blamer_enabled = 1
			vim.g.blamer_delay = 200
		end,
	},
	-- auto comment out
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
	-- buffer sizer
	"simeji/winresizer",
	-- show diff from recover file
	"chrisbra/Recover.vim",
	-- keep lastest cursor position
	"ethanholz/nvim-lastplace",
	-- mkdir
	"jghauser/mkdir.nvim",
	{
		"folke/todo-comments.nvim",
		config = function()
			local status, todocomments = pcall(require, "todo-comments")
			if not status then
				return
			end

			todocomments.setup({})
			local opts = { noremap = true, silent = true }
			vim.keymap.set("n", "<Leader>tq", "<cmd>TodoQuickFix<cr>", opts)
		end,
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
}


return spec
