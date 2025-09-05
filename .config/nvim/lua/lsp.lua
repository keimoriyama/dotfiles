-- lua_add {{{
local opts = { noremap = true, silent = true }

vim.lsp.config("lua_ls", {
	cmd = { "lua-language-server" },
	filetypes = { "lua" },
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim", "hs", "wez" },
			},
		},
	},
})

vim.lsp.config("pyright", {
	root_makers = { ".venv" },
	settings = {
		python = {
			venvPath = ".",
			pythonPath = "./.venv/bin/python",
			analysis = {
				extraPaths = { "." },
				diagnosticMode = "off",
				typeCheckingMode = "off",
			},
		},
	},
})

vim.lsp.config("ruff", { root_makers = { ".venv" } })

vim.lsp.config("denols", {
	root_makers = { "deno.json" },
	filetypes = { "typescript" },
	init_options = {
		lint = true,
		unstable = false,
		suggest = {
			imports = {
				hosts = {
					["https://deno.land"] = true,
					["https://cdn.nest.land"] = true,
					["https://crux.land"] = true,
				},
			},
		},
	},
})

vim.lsp.config("ts_ls", {
	filetypes = { "typescript" },
	root_dir = { "package.json" },
	root_makers = { "package.json" },
})

vim.lsp.config("lua_ls", {
	settings = {
		Lua = {
			semantic = { enable = false },
		},
	},
})

-- vim.lsp.config("copilot")
-- vim.lsp.config("nixd")

vim.lsp.enable({
	"lua_ls",
	"ruff",
	"pyright",
	"ty",
	"denols",
	"ts_ls",
	"rust_analyzer",
	"copilot",
	"nixd",
})

vim.lsp.handlers["textDocument/publishDiagnostics"] =
	vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { virtual_text = true })

-- vim.diagnostics.config({ severity_sort = true })
vim.diagnostic.config({
	virtual_text = true,
})
vim.lsp.inline_completion.enable(true)
-- LSP handlers
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
		vim.keymap.set("n", "<Leader>bf", "<cmd>lua vim.lsp.buf.format({async=true})<CR>", opt)
		vim.keymap.set("n", "<Leader>ic", vim.lsp.buf.incoming_calls, opt)
		vim.keymap.set("n", "[e", vim.diagnostic.goto_next, opt)
		vim.keymap.set("n", "]e", vim.diagnostic.goto_prev, opt)
		vim.keymap.set("n", "<Leader>e", vim.diagnostic.open_float, opts)
		vim.keymap.set("i", "<C-CR>", vim.lsp.inline_completion.get, opts)
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
-- }}}
