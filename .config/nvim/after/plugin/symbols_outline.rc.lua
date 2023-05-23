local status, outline = pcall(require, "symbols-outline")

if not status then
	return
end

outline.setup()

vim.keymap.set("n", "<C-o>", "<cmd>SymbolsOutline<CR>")
