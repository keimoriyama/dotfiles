vim.fn["ddu#custom#patch_global"]({
	ui = "filer",
	actionOptions = {
		narrow = {
			quit = [[v:false]],
		},
	},
	sources = {
		{ name = "file", params = {} },
	},
	sourceOptions = {
		_ = {
			columns = { "filename" },
		},
	},
	kindOptions = {
		file = {
			defaultAction = "open",
		},
	},
})

local opt = { buffer = true, silent = true }

vim.api.nvim_create_autocmd({ "Filetype" }, {
	pattern = { "ddu-filer" },
	callback = function()
		vim.keymap.set("n", "<CR>", '<cmd>call ddu#ui#filer#do_action("itemAction")<CR>', opt)
		vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#filer#do_action("toggleSelectItem")<CR>', opt)
		vim.keymap.set("n", "o", '<cmd>call ddu#ui#filer#do_action("expandItem", {"mode": "toggle"})<CR>', opt)
		vim.keymap.set("n", "q", '<cmd>call ddu#ui#filer#do_action("quit")<CR>', opt)
	end,
})
