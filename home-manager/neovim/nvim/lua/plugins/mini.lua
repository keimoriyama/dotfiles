local MiniIcons = require("mini.icons")
MiniIcons.setup({})
MiniIcons.tweak_lsp_kind()
MiniIcons.mock_nvim_web_devicons()

local MiniNotify = require("mini.notify")
MiniNotify.setup({})
vim.notify = MiniNotify.make_notify()

require("mini.comment").setup()
require("mini.pairs").setup()
require("mini.surround").setup()
require("mini.ai").setup()
require("mini.jump").setup()
require("mini.diff").setup()
require("mini.git").setup()
require("mini.tabline").setup()
require("mini.jump").setup()
require("mini.pairs").setup()
require("mini.diff").setup({
	view = {
		-- Visualization style. Possible values are 'sign' and 'number'.
		-- Default: 'number' if line numbers are enabled, 'sign' otherwise.
		style = vim.go.number and "number" or "sign",
	},
})
require("mini.bracketed").setup()
local miniclue = require("mini.clue")
miniclue.setup({
	triggers = {
		-- Leader triggers
		{ mode = { "n", "x" }, keys = "<Leader>" },

		-- `[` and `]` keys
		{ mode = "n", keys = "[" },
		{ mode = "n", keys = "]" },

		-- Built-in completion
		{ mode = "i", keys = "<C-x>" },

		-- `g` key
		{ mode = { "n", "x" }, keys = "g" },

		-- Marks
		{ mode = { "n", "x" }, keys = "'" },
		{ mode = { "n", "x" }, keys = "`" },

		-- Registers
		{ mode = { "n", "x" }, keys = '"' },
		{ mode = { "i", "c" }, keys = "<C-r>" },

		-- Window commands
		{ mode = "n", keys = "<C-w>" },

		-- `z` key
		{ mode = { "n", "x" }, keys = "z" },
	},

	clues = {
		-- Enhance this by adding descriptions for <Leader> mapping groups
		miniclue.gen_clues.square_brackets(),
		miniclue.gen_clues.builtin_completion(),
		miniclue.gen_clues.g(),
		miniclue.gen_clues.marks(),
		miniclue.gen_clues.registers(),
		miniclue.gen_clues.windows(),
		miniclue.gen_clues.z(),
	},
})

local MiniHipatterns = require("mini.hipatterns")
MiniHipatterns.setup({
	highlighters = {
		fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
		hack = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
		todo = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
		note = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },
		hex_color = MiniHipatterns.gen_highlighter.hex_color(),
	},
})

require("plugins.completion.mini_snippets")
require("plugins.completion.mini_completion")
require("plugins.fuzzy.mini_pick")
