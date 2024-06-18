-- lua_source{{{

vim.api.nvim_create_autocmd({ "FileType" }, {
	pattern = { "deol" },
	callback = function()
		local keymap = vim.keymap.set
		local opts = { buffer = true, noremap = true }
		keymap("n", "<C-n>", "<Plug>(deol_next_prompt)", opts)
	end,
})

vim.api.nvim_create_autocmd({ "Bufenter" }, {
	pattern = "deol-edit@default",
	callback = function()
		local keymap = vim.keymap.set
		local opts = { buffer = true, noremap = true }
		keymap("n", "<CR>", "<Plug>(deol_execute_line)", opts)
	end,
})
-- }}}
