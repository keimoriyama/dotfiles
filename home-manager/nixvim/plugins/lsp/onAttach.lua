local client = vim.lsp.get_client_by_id(ev.data.client_id)
if client and client.server_capabilities.documentHighlightProvider then
	vim.api.nvim_command("set updatetime=100")
	vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
	vim.api.nvim_clear_autocmds({
		buffer = ev.buf,
		group = "lsp_document_highlight",
	})
	vim.api.nvim_create_autocmd("CursorHold", {
		callback = vim.lsp.buf.document_highlight,
		buffer = ev.buf,
		group = "lsp_document_highlight",
		desc = "Document Highlight",
	})
	vim.api.nvim_create_autocmd("CursorMoved", {
		callback = vim.lsp.buf.clear_references,
		buffer = ev.buf,
		group = "lsp_document_highlight",
		desc = "Clear All the References",
	})
end
