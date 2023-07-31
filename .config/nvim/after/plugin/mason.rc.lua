---@diagnostic disable: redefined-local
local status, mason = pcall(require, "mason")
if not status then
	return
end

mason.setup({
	ui = {
		icons = {
			package_installed = "✓",
			package_pending = "➜",
			package_uninstalled = "✗",
		},
	},
})

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "<Leader>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "<Leader>q", vim.diagnostic.setloclist, opts)

-- add lsp
local servers = { "pyright", "lua_ls", "texlab", "clangd", "html" }
local status, mason_lspconfig = pcall(require, "mason-lspconfig")
if not status then
	return
end

mason_lspconfig.setup({ ensure_installed = servers })

local status, lspconfig = pcall(require, "lspconfig")
if not status then
	return
end

local handlers = {
	function(server_name)
		lspconfig[server_name].setup({})
	end,
}
mason_lspconfig.setup_handlers(handlers)

-- -- LSP handlers
vim.lsp.handlers["textDocument/publishDiagnostics"] =
	vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { virtual_text = true })

vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("UserLspConfig", {}),
	callback = function(ev)
		-- Enable completion triggered by <c-x><c-o>
		vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
		local opt = { noremap = true, silent = true, buffer = ev.buf }
		-- Mappings.
		-- See `:help vim.lsp.*` for documentation on any of the below functions
		vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opt)
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, opt)
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opt)
		vim.keymap.set("n", "gr", vim.lsp.buf.references, opt)
		vim.keymap.set("n", "H", vim.lsp.buf.hover, opt)
		vim.keymap.set("n", "K", vim.lsp.buf.type_definition, opt)
		vim.keymap.set("n", "<Leader>D", vim.lsp.buf.type_definition, opt)
		vim.keymap.set("n", "<Leader>rn", vim.lsp.buf.rename, opt)
		vim.keymap.set("n", "<Leader>f", "<cmd>lua vim.lsp.buf.format({async=true})<CR>", opt)
		vim.keymap.set("n", "<Leader>cc", vim.lsp.buf.incoming_calls, opt)
		vim.keymap.set("n", "[e", vim.diagnostic.goto_next, opt)
		vim.keymap.set("n", "]e", vim.diagnostic.goto_prev, opt)
		-- Reference highlight
		local client = vim.lsp.get_client_by_id(ev.data.client_id)
		if client.server_capabilities.documentHighlightProvider then
			vim.api.nvim_command("highlight LspReferenceText  cterm=underline ctermbg=8 gui=underline guibg=#104040")
			vim.api.nvim_command("highlight LspReferenceRead  cterm=underline ctermbg=8 gui=underline guibg=#104040")
			vim.api.nvim_command("highlight LspReferenceWrite cterm=underline ctermbg=8 gui=underline guibg=#104040")
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
	end,
})
-- LSP handlers
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = "single", -- "shadow" , "none", "rounded"
})
