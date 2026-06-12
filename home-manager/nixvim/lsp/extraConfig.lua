vim.lsp.inline_completion.enable(true)

vim.keymap.set("i", "<C-CR>", "<cmd>lua vim.lsp.inline_completion.get()<cr>", {
	silent = true,
	desc = "Get inline completion from LSP",
})

vim.api.nvim_create_autocmd("BufWritePre", {
	callback = function()
		vim.lsp.buf.format({ async = false })
	end,
})
