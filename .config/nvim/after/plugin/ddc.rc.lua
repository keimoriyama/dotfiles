-- -- -- Use around source.
vim.fn["ddc#custom#patch_global"]({
	ui = "pum",
	sources = { "around", "nvim-lsp", "file", "buffer", "copilot" },
	autoCompleteEvents = {
		"InsertEnter",
		"TextChangedI",
		"TextChangedP",
		"CmdlineChanged",
	},
	cmdlineSources = {
		[":"] = { "cmdline", "cmdline-history", "around" },
	},
})

-- コマンドライン補完の設定
vim.cmd([[
	nnoremap :       <Cmd>call CommandlinePre()<CR>:

	function! CommandlinePre() abort
		cnoremap <Tab>   <Cmd>call pum#map#insert_relative(+1)<CR>
		cnoremap <S-Tab> <Cmd>call pum#map#insert_relative(-1)<CR>
		cnoremap <C-n>   <Cmd>call pum#map#insert_relative(+1)<CR>
		cnoremap <C-p>   <Cmd>call pum#map#insert_relative(-1)<CR>
		cnoremap <C-y>   <Cmd>call pum#map#confirm()<CR>
		cnoremap <C-e>   <Cmd>call pum#map#cancel()<CR>

		autocmd User DDCCmdlineLeave ++once call CommandlinePost()

		" Enable command line completion for the buffer
		call ddc#enable_cmdline_completion()
	endfunction
	function! CommandlinePost() abort
		silent! cunmap <Tab>
		silent! cunmap <S-Tab>
		silent! cunmap <C-n>
		silent! cunmap <C-p>
		silent! cunmap <C-y>
		silent! cunmap <C-e>
	endfunction
]])

vim.fn["ddc#custom#patch_global"]("sourceOptions", {
	["around"] = { mark = "around" },
	["_"] = {
		matchers = { "matcher_head", "matcher_fuzzy", "matcher_length" },
		sorters = { "sorter_fuzzy", "sorter_rank" },
		converters = { "converter_fuzzy" },
	},
	["nvim-lsp"] = { mark = "lsp", forceCompletionPattern = [['\.\w*|:\w*|->\w*']] },
	["file"] = { mark = "file", isVolatile = [[v:true]], forceCompletionPattern = [['\S/\S*']] },
	["cmdline"] = { mark = "cmdline" },
	["buffer"] = { mark = "buffer" },
})

vim.fn["ddc#custom#patch_global"]("sourceParams", {
	["around"] = { maxSize = 500 },
	["nvim-lsp"] = { kindLabels = { Class = "c" } },
	["buffer"] = { requireSameFiletype = [[v:false]], forceCollect = [[v:true]] },
	["copilot"] = { mark = "copilot", minAutoCompleteLength = 1 },
})

vim.fn["ddc#custom#patch_global"]("filterParams", {
	["matcher_fuzzy"] = { camelcase = [[v:true]] },
})

-- path completion
vim.fn["ddc#custom#patch_filetype"]({ "ps1", "dosbatch", "autohotkey", "registry" }, {
	sourceOptions = {
		file = {
			forceCompletionPattern = [['\S\\\S*']],
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
-- -- Use ddc.
vim.fn["ddc#enable"]()

-- Use signature help
vim.fn["signature_help#enable"]()

-- use pop up preview
vim.fn["popup_preview#enable"]()
