--- lua_source{{{
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

-- add lsp
local servers = {
	"lua_ls",
	"html",
	"jsonls",
}

local status, mason_lspconfig = pcall(require, "mason-lspconfig")
if not status then
	return
end
mason_lspconfig.setup({ ensure_installed = servers })
---}}}
