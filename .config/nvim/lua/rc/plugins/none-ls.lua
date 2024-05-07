local M = {}
local add, later = MiniDeps.add, MiniDeps.later

function M.setup()
	later(function()
		add({ source = "nvimtools/none-ls.nvim", depends = { "nvim-lua/plenary.nvim" } })
		none_ls_config()
	end)
end

function none_ls_config()
	-- lua_source {{{
	local status, null_ls = pcall(require, "null-ls")
	if not status then
		return
	end

	local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

	local on_attach = function(client, bufnr)
		-- you can reuse a shared lspconfig on_attach callback here
		if client.supports_method("textDocument/formatting") then
			vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
			vim.api.nvim_create_autocmd("BufWritePre", {
				group = augroup,
				buffer = bufnr,
				callback = function()
					vim.lsp.buf.format({ bufnr = bufnr })
				end,
			})
		end
	end

	null_ls.setup({
		debug = false,
		sources = {
			null_ls.builtins.formatting.stylua,
			-- null_ls.builtins.formatting.black,
			-- null_ls.builtins.formatting.isort,
			null_ls.builtins.formatting.prettier,
		},
		on_attach = on_attach,
	})
end

return M
