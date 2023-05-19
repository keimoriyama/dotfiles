local status, aerial = pcall(require, "aerial")
if not status then
	return
end

aerial.setup({
	backends = { "treesitter", "lsp", "man", "markdown" },
	layout = {
		max_width = { 60, 0.4 },
		default_direction = "prefer_left",
	},
	filter_kind = {
		"Class",
		"Constructor",
		"Enum",
		"Function",
		"Interface",
		"Module",
		"Method",
		"Struct",
	},
})

vim.g.mapleader = " "
-- You probably also want to set a keymap to toggle aerial
vim.keymap.set("n", "<leader>a", "<cmd>AerialToggle<CR>")
