function Setup_linter()
	local status, lint = pcall(require, "lint")
	if not status then
		return
	end
	-- add
	local status, mason_package = pcall(require, "mason-core.package")
	if not status then
		return
	end
	local status, mason_registry = pcall(require, "mason-registry")
	if not status then
		return
	end
	local sources = {}
	for _, package in ipairs(mason_registry.get_installed_packages()) do
		local package_categories = package.spec.categories[1]
		if package_categories == mason_package.Cat.Linter then
			local linter_name = package.name
			for _, language in ipairs(package.spec.languages) do
				-- sources[string.lower(language)] = { linter_name }
				local lang = string.lower(language)
				local linter_by_lang = lint.linters_by_ft[lang]
				if linter_by_lang == nil then
					linter_by_lang = {}
				end
				if lint.linters[linter_name] == nil then
					error("This linter " .. linter_name .. " is not available in nvim-lint!!!")
				else
					table.insert(linter_by_lang, linter_name)
					sources[lang] = linter_by_lang
				end
			end
		end
	end
	lint.linters_by_ft = sources

	-- print(vim.inspect(sources))
	vim.api.nvim_create_autocmd({ "BufWritePre" }, {
		callback = function()
			lint.try_lint()
		end,
	})
end

function Setup_formatter()
	local status, formatter = pcall(require, "formatter")
	if not status then
		return
	end
	local status, filetypes = pcall(require, "formatter.filetypes")
	if not status then
		return
	end

	local status, mason_package = pcall(require, "mason-core.package")
	if not status then
		return
	end
	local status, mason_registry = pcall(require, "mason-registry")
	if not status then
		return
	end

	local formatters = {}

	for _, package in ipairs(mason_registry.get_installed_packages()) do
		local package_categories = package.spec.categories[1]
		if package_categories == mason_package.Cat.Formatter then
			local formatter_name = package.name
			for _, language in ipairs(package.spec.languages) do
				local lang = string.lower(language)
				if filetypes[lang] == nil then
					error("This lang " .. lang .. " is not available in nvim-formatter!!!")
					goto continue
				end

				if filetypes[lang][formatter_name] == nil then
					-- print("This formatter " .. formatter .. " is not available in nvim-formatter!!!")
					goto continue
				else
					local lang_table = formatters[lang]
					if lang_table == nil then
						lang_table = {}
					end
					local formatter = filetypes[lang][formatter_name]

					table.insert(lang_table, formatter)
					formatters[lang] = lang_table
				end
				::continue::
			end
		end
	end
	-- print(vim.inspect(formatters))
	formatter.setup({
		filetype = formatters,
	})
	vim.api.nvim_create_autocmd("BufWritePost", {
		pattern = { "*" },
		command = "FormatWrite",
	})
end

---@type LazySpec
local spec = {
	{
		"williamboman/mason.nvim",
		dependencies = {
			-- lsp
			"neovim/nvim-lspconfig",
			"williamboman/mason-lspconfig.nvim",
			-- linter
			"mfussenegger/nvim-lint",
			-- formatter
			"mhartington/formatter.nvim",
		},
		config = function()
			-- linter,formatter,lspの設定が書いてある
			--print("mason")
			---@diagnostic disable: redefined-local
			local status, mason = pcall(require, "mason")
			local nvim_lsp = require("lspconfig")
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

			-- add lsp
			local servers = { "pyright", "lua_ls", "texlab", "clangd", "html", "rust_analyzer" }
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
					local node_root_dir = nvim_lsp.util.root_pattern("package.json")
					local is_node_repo = node_root_dir(vim.api.nvim_buf_get_name(0)) ~= nil
					local opts = {}
					if server_name == "tsserver" then
						if not is_node_repo then
							return
						end
					elseif server_name == "eslint" then
						if not is_node_repo then
							return
						end
					elseif server_name == "denols" then
						if is_node_repo then
							return
						end
					end
					opts.root_dir = nvim_lsp.util.root_pattern("deno.json", 'deno.jsonc', 'deps.ts', 'import_map.json')
					opts.init_options = {
						lint = true,
						unstable = true,
						suggest = {
							imports = {
								hosts = {
									["https://deno.land"] = true,
									["https://cdn.nest.land"] = true,
									["https://crux.land"] = true
								}
							}
						}
					}
					lspconfig[server_name].setup(opts)
				end,
			}
			mason_lspconfig.setup_handlers(handlers)

			-- -- LSP handlers
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
					-- Reference highlight
					local client = vim.lsp.get_client_by_id(ev.data.client_id)
					if client.server_capabilities.documentFormattingProvider then
						vim.api.nvim_create_autocmd({ "BufWritePre" }, {
							buffer = bufnr,
							callback = function()
								vim.lsp.buf.format({ timeout_ms = 2500 })
							end,
						})
					end
					if client.server_capabilities.documentHighlightProvider then
						vim.api.nvim_command(
							"highlight LspReferenceText  cterm=underline ctermbg=8 gui=underline guibg=#104040"
						)
						vim.api.nvim_command(
							"highlight LspReferenceRead  cterm=underline ctermbg=8 gui=underline guibg=#104040"
						)
						vim.api.nvim_command(
							"highlight LspReferenceWrite cterm=underline ctermbg=8 gui=underline guibg=#104040"
						)
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
			-- LSP handlers
			vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
				border = "single", -- "shadow" , "none", "rounded"
			})
			Setup_formatter()
			Setup_linter()
		end,
	},
}

return spec
