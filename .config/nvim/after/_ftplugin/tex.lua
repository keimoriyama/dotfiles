local status, cmp = pcall(require, "cmp")
if not status then
	return
end

local lspkind = require("lspkind")
cmp.setup({
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	sources = {
		{ name = "luasnip", option = { show_autosnippets = true } },
		{ name = "omni", keyword_length = 0 },
		{ name = "nvim_lsp" },
		{ name = "buffer" },
		{ name = "mocword" },
		{ name = "path" },
		{ name = "nvim_lsp_signature_help" },
		{ name = "treesitter" },
		{ name = "copilot" },
	},
	completion = { keyword_length = 1, completeopt = "menu,noselect" },
	view = { entries = "custom" },
	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol_text",
			menu = {
				nvim_lsp = "[LSP]",
				ultisnips = "[US]",
				nvim_lua = "[Lua]",
				path = "[Path]",
				buffer = "[Buffer]",
				emoji = "[Emoji]",
				omni = "[Omni]",
			},
		}),
	},
})
