local M = {}


function M.setup()
	local capabilities = require("ddc_nvim_lsp").make_client_capabilities()
	capabilities.textDocument.completion.completionItem.snippetSupport = true
	require("lspconfig").denols.setup({
		capabilities = capabilities,
	})

	-- Use around source.
	vim.fn["ddc#custom#patch_global"]({
		ui = "pum",
		sources = {
			"vsnip",
			"nvim-lsp",
			"file",
			"nvim-lua",
			"buffer",
			"around",
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
			around = { mark = "[around]" },
			file = { mark = "[file]", isVolatile = true, forceCompletionPattern = [['\S/\S*']] },
			cmdline = { mark = "[cmdline]" },
			buffer = { mark = "[buffer]" },
			["nvim-lsp"] = {
				mark = "[lsp]",
				forceCompletionPattern = [['\.\w*|:\w*|->\w*']],
				keywordPattern = "\\k*",
				dup = "force",
			},
			["nvim-lua"] = { mark = "[lua]", forceCompletionPattern = "." },
			vsnip = { mark = "[vsnip]" },
			skkeleton = {
				mark = "skk",
				matchers = { "skkeleton" },
				sorters = {},
				minAutoCompleteLength = 1,
				isVolatile = true
			},
			["cmdline-history"] = { mark = "[cmdline-history]" },
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
end

return M
