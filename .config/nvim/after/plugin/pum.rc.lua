vim.keymap.set({ "i", "c" }, "<C-n>", "<cmd>call pum#map#insert_relative(+1, 'loop')<cr>")
vim.keymap.set({ "i", "c" }, "<C-p>", "<cmd>call pum#map#insert_relative(-1, 'loop')<cr>")
vim.keymap.set("i", "<C-y>", "<cmd>call pum#map#confirm()<cr>")
vim.keymap.set("i", "<C-e>", "<cmd>call pum#map#cancel()<cr>")
vim.fn["pum#set_option"]({
	auto_select = true,
	item_orders = { "kind", "abbr", "menu" },
	highlight_selected = "CursorLine",
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
