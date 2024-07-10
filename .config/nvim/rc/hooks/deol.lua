-- lua_source{{{
vim.api.nvim_create_autocmd({ "FileType" }, {
	pattern = { "deol" },
	callback = function()
		local keymap = vim.keymap.set
		local opts = { buffer = true, noremap = true }
		keymap("n", "<C-n>", "<Plug>(deol_next_prompt)", opts)
		keymap("n", "<C-p>", "<Plug>(deol_previous_prompt)", opts)
		keymap("n", "<CR>", "<Plug>(deol_execute_line)", opts)
		keymap("n", "A", "<Plug>(deol_start_append_last)", opts)
		keymap("n", "I", "<Plug>(deol_start_insert_first)", opts)
		keymap("n", "a", "<Plug>(deol_start_append)", opts)
		keymap("n", "e", "<Plug>(deol_edit)", opts)
		keymap("n", "i", "<Plug>(deol_start_insert)", opts)
		keymap("n", "q", "<Plug>(deol_quit)", opts)
	end,
})

vim.api.nvim_create_autocmd({ "Bufenter" }, {
	pattern = "deol-edit@default",
	callback = function()
		local keymap = vim.keymap.set
		local opts = { buffer = true, noremap = true }
		keymap({ "n", "i" }, "<CR>", "<Plug>(deol_execute_line)", opts)
		keymap({ "n", "i" }, "<BS>", "<Plug>(deol_backspace)", opts)
		keymap({ "n", "i" }, "<C-h>", "<Plug>(deol_backspace)", opts)
		keymap("n", "q", "<Plug>(deol_quit)", opts)
		keymap({ "n", "i" }, "<C-c>", "<Plug>(deol_ctrl_c)", opts)
		keymap("n", "<C-d>", "<Plug>(deol_quit)", opts)
	end,
})

local opts = { buffer = true, noremap = true }
vim.fn["deol#set_option"]({
	split = "horizontal",
	toggle = true,
	internal_history_path = vim.fn.expand("~/.cache/deol-history"),
})
vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>call deol#start()<CR>", opts)
-- }}}
