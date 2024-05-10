local M = {}

function M.setup()
	require("rc.plugins.utils").setup()
	require("rc.plugins.lsp").setup()
	require("rc.plugins.none-ls").setup()
	require("rc.plugins.luasnip").setup()
	require("rc.plugins.cmp").setup()
	require("rc.plugins.telescope").setup()
	require("rc.plugins.treesitter").setup()
	require("rc.plugins.obsidian").setup()
	require("rc.plugins.skk").setup()
	require("rc.plugins.vimtex").setup()
	require("rc.plugins.peek").setup()
end

return M
