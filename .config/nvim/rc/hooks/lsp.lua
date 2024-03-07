-- lua_source {{{
---@disable: redefined-local
vim.keymap.set("n", "<leader>ll", "<cmd>LspInfo<cr>")
local opts = { noremap = true, silent = true }

-- https://github.com/neovim/neovim/issues/23291#issuecomment-1523243069
-- https://github.com/neovim/neovim/pull/23500#issuecomment-1585986913
-- pyright asks for every file in every directory to be watched,
-- so for large projects that will necessarily turn into a lot of polling handles being created.
-- sigh
local ok, wf = pcall(require, "vim.lsp._watchfiles")
if ok then
	wf._watchfunc = function()
		return function() end
	end
end

local nvim_lsp = require("lspconfig")

nvim_lsp.lua_ls.setup({
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim", "hs", "wez" },
			},
		},
	},
})

nvim_lsp.pyright.setup({
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

local function ruff_lsp_on_attatch()
	vim.api.nvim_create_autocmd({ "BufWritePre" }, {
		buffer = bufnr,
		callback = function()
			vim.lsp.buf.format({ timeout_ms = 2500 })
		end,
	})
end

nvim_lsp.ruff_lsp.setup({
	on_attach = ruff_lsp_on_attatch,
	init_options = {
		settings = {
			-- Any extra CLI arguments for `ruff` go here.
			args = {},
		},
	},
})

nvim_lsp.denols.setup({
	root_dir = nvim_lsp.util.root_pattern("deno.json"),
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

nvim_lsp.tsserver.setup({
	root_dir = nvim_lsp.util.root_pattern("package.json"),
})
-- LSP handlers
vim.diagnostic.config({ virtual_text = false })
vim.lsp.handlers["textDocument/publishDiagnostics"] =
	vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { virtual_text = false })

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
		local client = vim.lsp.get_client_by_id(ev.data.client_id)
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
vim.api.nvim_create_autocmd("CursorHold", {
	callback = function()
		vim.diagnostic.open_float({ focus = false })
	end,
})
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = "single", -- "shadow" , "none", "rounded"
})
-- }}}

-- lua_python {{{
local cli_path = os.getenv("BASE_DIR") .. "/rc/hooks/python/.venv/bin"
vim.env.PATH = vim.env.PATH .. ":" .. cli_path
-- }}}

-- lua_typescript_javascript{{{
local dir = os.getenv("BASE_DIR") .. "/rc/hooks/node/node_modules/.bin"
vim.env.PATH = vim.env.PATH .. ":" .. dir
vim.system({ "bun", "i" }, { cwd = dir, text = true })
-- }}}

-- lua_post_update {{{
-- print("updating python hooks")
vim.system({ "rye", "sync" }, { cwd = os.getenv("BASE_DIR") .. "/rc/hooks/python", stdout = false })
-- print("updating typescript hooks")
vim.system({ "bun", "install" }, { cwd = os.getenv("BASE_DIR") .. "/rc/hooks/node", stdout = false })
-- }}}
