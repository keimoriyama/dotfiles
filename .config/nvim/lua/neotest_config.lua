-- lua_source {{{
print("neotest config is loaded")
require("neotest").setup({
	running = {
		-- Run tests concurrently when an adapter provides multiple commands to run.
		concurrent = true,
	},
	summary = {
		-- Enable/disable animation of icons.
		animated = false,
	},
	adapters = {
		require("neotest-plenary"),
		require("neotest-python"),
	},
})
-- }}}
