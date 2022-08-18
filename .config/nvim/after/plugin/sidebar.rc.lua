local sidebar = require("sidebar-nvim")
local opts = {
	open = true,
	sections = { "datetime", "git", "diagnostics", "files" },
	files = {
		icon = "",
		show_hidden = false,
		ignored_paths = { "%.git$" }
	},
	bindings = { ["q"] = function() require("sidebar-nvim").close() end }
}
sidebar.setup(opts)
vim.keymap
	.set('n', '<leader>n', sidebar.toggle, { noremap = true, silent = true })
