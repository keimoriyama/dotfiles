local status, outline = pcall(require, "symbols-outline")

if not status then
	return
end

outline.setup({
	position = "right",
	auto_close = true,
	show_number = true,
	show_symbol_details = true,
})

vim.keymap.set("n", "<C-o>", "<cmd>SymbolsOutline<CR>")
