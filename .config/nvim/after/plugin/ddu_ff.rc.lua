vim.fn["ddu#custom#patch_global"]({
	ui = "ff",
	sources = {
		{
			name = "file_rec",
			params = {},
		},
	},
	sourceOptions = {
		_ = {
			matchers = { "matcher_substring" },
		},
	},
	kindOptions = {
		file = {
			defaultAction = "open",
		},
	},
	uiParams = {
		ff = {
			split = "floating",
			startFilter = [[v:true]],
		},
	},
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-ff",
	callback = function()
		local opt = { buffer = true, silent = true }
		vim.keymap.set("n", "<CR>", '<cmd>call ddu#ui#filer#do_action("itemAction", {"name": "open"})<CR>', opt)
		vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#filer#do_action("toggleSelectItem")<CR>', opt)
		vim.keymap.set("n", "i", '<cmd>call ddu#ui#do_action("openFilterWindow")<CR>', opt)
		vim.keymap.set("n", "q", '<cmd>call ddu#ui#do_action("quit")<CR>', opt)
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-ff-filter",
	callback = function()
		local opt = { buffer = true, silent = true }
		vim.keymap.set("i", "<CR>", "<esc><cmd>close<CR>", opt)
		vim.keymap.set("n", "<CR>", "<cmd>close<CR>", opt)
		vim.keymap.set("q", "<CR>", "<cmd>close<CR>", opt)
	end,
})
