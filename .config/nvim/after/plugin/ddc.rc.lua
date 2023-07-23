local capabilities = require("ddc_nvim_lsp").make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
require("lspconfig").denols.setup({
	capabilities = capabilities,
})

-- Use around source.
vim.fn["ddc#custom#patch_global"]({
	ui = "pum",
	sources = {
		"nvim-lsp",
		"file",
		"vsnip",
		"nvim-lua",
		"buffer",
		"around",
		"copilot",
	},
	autoCompleteEvents = {
		"InsertEnter",
		"TextChangedI",
		"TextChangedP",
		"CmdlineEnter",
		"CmdlineChanged",
		"TextChangedT",
	},
	cmdlineSources = {
		[":"] = { "cmdline", "cmdline-history", "around" },
	},
	sourceOptions = {
		["_"] = {
			matchers = { "matcher_head", "matcher_fuzzy" },
			sorters = { "sorter_rank" },
			converters = { "converter_fuzzy", "converter_remove_overlap", "converter_truncate_abbr" },
			minAutoCompleteLength = 1,
		},
		around = { mark = "a" },
		file = { mark = "f", isVolatile = true, forceCompletionPattern = [['\S/\S*']] },
		cmdline = { mark = "c" },
		buffer = { mark = "b" },
		["nvim-lsp"] = {
			mark = "lsp",
			forceCompletionPattern = [['\.\w*|:\w*|->\w*']],
			keywordPattern = "\\k*",
			dup = "force",
		},
		["nvim-lua"] = { mark = "lua", forceCompletionPattern = "." },
		vsnip = { mark = "vsnip" },
		copilot = {
			mark = "copilot",
			matchers = {},
			minAutoCompleteLength = 1,
		},
	},
	sourceParams = {
		buffer = { requireSameFiletype = false, forceCollect = true },
		["nvim-lsp"] = {
			enableResolveItem = true,
			enableAdditionalTextEdit = true,
			confirmBehavior = "insert",
			snippetEngine = vim.fn["denops#callback#register"](function(body)
				return vim.fn["vsnip#anonymous"](body)
			end),
		},
	},
})

-- path completion
vim.fn["ddc#custom#patch_filetype"]({ "ps1", "dosbatch", "autohotkey", "registry" }, {
	sourceOptions = {
		file = {
			forceCompletionPattern = [['\S\\\S*']],
			minAutoCompleteLength = 1,
		},
	},
	sourceParams = {
		file = {
			mode = [[win32]],
		},
	},
})

vim.g.signature_help_config = {
	contentsStyle = "currentLabel",
	viewStyle = "floating",
}

-- Use ddc.
vim.fn["ddc#enable"]()
-- Use signature help
vim.fn["signature_help#enable"]()
-- use pop up preview
vim.fn["popup_preview#enable"]()
