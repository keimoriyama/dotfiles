local M = {}
function M.setup()
	vim.fn["pum#set_option"]({
		item_orders = { "abbr", "kind", "menu" },
		highlight_selected = "CursorLine",
		horizontal_menu = false,
		offset_cmdcol = 0,
		padding = false,
		use_complete = true,
		use_setline = false,
	})

	vim.keymap.set({ "i", "c" }, "<C-n>", "<cmd>call pum#map#insert_relative(+1, 'loop')<cr>")
	vim.keymap.set({ "i", "c" }, "<C-p>", "<cmd>call pum#map#insert_relative(-1, 'loop')<cr>")
	vim.keymap.set("i", "<C-y>", "<cmd>call pum#map#confirm()<cr>")
	vim.keymap.set("i", "<C-e>", "<cmd>call pum#map#cancel()<cr>")

	-- コマンドライン補完の設定
	-- CommandlinePost()とCommandlinePre()のキーマップは必ず揃える
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
end

return M
