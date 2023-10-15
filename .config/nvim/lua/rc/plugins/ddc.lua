---@type LazySpec
local spec = {
	{
		"Shougo/ddc.vim",
		-- event = { 'InsertEnter', "CmdlineEnter" },
		dependencies = {
			'vim-denops/denops.vim',
		},
		init = function()
			vim.g.dps_obsidian_base_dir = "~/Documents/Notes/"
		end,
		config = function()
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
					'copilot',
					"nvim-lua",
					"buffer",
					"around",
					'obsidian',
					'obsidian_new',
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
					copilot = { mark = "[copilot]", minAutoCompleteLength = 1, isVolatile = true },
					["cmdline-history"] = { mark = "[cmdline-history]" },
					obsidian = { mark = "[obsidian]", keywordPattern = '[[.*?]]' },
					obsidian_new = { mark = "[obsidian_new]", keywordPattern = '[[.*?]]' },
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
			-- -- Use ddc
			vim.fn["ddc#enable"]()
			require("ddc_previewer_floating").enable()
		end
	},
	"Shougo/ddc-nvim-lsp",
	"Shougo/ddc-around",
	"LumaKernel/ddc-file",
	"matsui54/ddc-buffer",
	"Shougo/ddc-source-cmdline",
	"Shougo/ddc-source-cmdline-history",
	"Shougo/ddc-source-nvim-lsp",
	"Shougo/ddc-source-copilot",
	"uga-rosa/ddc-source-nvim-lua",
	"uga-rosa/ddc-source-vsnip",
	"LumaKernel/ddc-source-file",
	"Shougo/ddc-sorter_rank",
	"Shougo/ddc-ui-pum",
	"Shougo/ddc-ui-native",
	"Shougo/ddc-converter_remove_overlap",
	"tani/ddc-fuzzy",
	"Shougo/ddc-matcher_head",
	"tani/ddc-path",
	"uga-rosa/ddc-nvim-lsp-setup",
	"Shougo/ddc-filter-converter_truncate_abbr",
	"lambdalisue/guise.vim",
	"uga-rosa/ddc-previewer-floating"
}


return spec
