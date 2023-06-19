-- -- Use around source.
vim.fn["ddc#custom#patch_global"]("ui", "pum")
vim.fn["ddc#custom#patch_global"]("sources", { "around", "nvim-lsp", "file", "buffer" })

-- vim.fn["ddc#custom#patch_global"](
-- 	"autoCompleteEvents",
-- 	{ "InsertEnter", "TextChangeI", "TextChangeP", "CmdlineChanged" }
-- )
--
-- vim.fn["ddc#custom#patch_global"]("cmdlineSources", { [":"] = { "cmdline", "cmdline-history", "around" } })

vim.fn["ddc#custom#patch_global"]("sourceOptions", {
	["around"] = { mark = "around" },
	["_"] = {
		matchers = { "matcher_head", "matcher_fuzzy", "matcher_length" },
		sorters = { "sorter_fuzzy", "sorter_rank" },
		converters = { "converter_fuzzy" },
	},
	["nvim-lsp"] = { mark = "lsp", forceCompletionPattern = [['\.\w*|:\w*|->\w*']] },
	["file"] = { mark = "file", isVolatile = [[v:true]], forceCompletionPattern = [['\S/\S*']] },
	["buffer"] = { mark = "buffer" },
})

vim.fn["ddc#custom#patch_global"]("sourceParams", {
	["around"] = { maxSize = 500 },
	["nvim-lsp"] = { kindLabels = { Class = "c" } },
	["buffer"] = { requireSameFiletype = [[v:false]], forceCollect = [[v:true]] },
})

vim.g.signature_help_config = {
	contentsStyle = "currentLabel",
	viewStyle = "floating",
}

vim.fn["ddc#custom#patch_global"]("filterParams", {
	["matcher_fuzzy"] = { camelcase = [[v:true]] },
})
-- -- Use ddc.
vim.fn["ddc#enable"]()

-- Use signature help
vim.fn["signature_help#enable"]()

-- use pop up preview
vim.fn["popup_preview#enable"]()
