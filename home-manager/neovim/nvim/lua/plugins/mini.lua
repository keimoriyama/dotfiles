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
