vim.lsp.inline_completion.enable(true)

vim.keymap.set("i", "<C-CR>", "<cmd>lua vim.lsp.inline_completion.get()<cr>", {
	silent = true,
	desc = "Get inline completion from LSP",
})
