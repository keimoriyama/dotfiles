---@type LazySpec
local spec = {
	-- auto pair and tag close
	{
		"windwp/nvim-ts-autotag",
		event = "InsertEnter",
	},
	-- surround
	{
		"kylechui/nvim-surround",
		config = function()
			require("nvim-surround").setup()
		end
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
	{
		"ethanholz/nvim-lastplace",
		config = function()
			require("nvim-lastplace").setup()
		end
	},
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
		end,
	},
	"LeafCage/vimhelpgenerator",
	'skanehira/denops-silicon.vim',
	{

		'phaazon/hop.nvim',
		config = function()
			local hop = require('hop')
			hop.setup { keys = 'etovxqpdygfblzhckisuran' }
			local directions = require('hop.hint').HintDirection
			vim.keymap.set('', 'f', function()
				hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
			end, { remap = true })
			vim.keymap.set('', 'F', function()
				hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
			end, { remap = true })
			vim.keymap.set('', 't', function()
				hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
			end, { remap = true })
			vim.keymap.set('', 'T', function()
				hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
			end, { remap = true })
		end
	}
}

return spec
