vim.fn["ddu#custom#patch_local"]("filer", {
	ui = "filer",
	sources = {
		{ name = "file", params = {} },
	},
	sourceOptions = {
		_ = {
			columns = { "filename" },
			sorters = { "sorter_alpha" },
		},
	},
	kindOptions = {
		file = {
			defaultAction = "open",
		},
	},
	uiParams = {
		filer = {
			split = "floating",
		},
	},
})

local opt = { buffer = true }

vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-filer",
	callback = function()
		vim.keymap.set("n", "<CR>", '<cmd>call ddu#ui#filer#do_action("itemAction", {"name": "open"})<CR>', opt)
		vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#filer#do_action("toggleSelectItem")<CR>', opt)
		vim.keymap.set("n", "o", '<cmd>call ddu#ui#do_action("expandItem", {"mode": "toggle"})<CR>', opt)
		vim.keymap.set("n", "q", '<cmd>call ddu#ui#do_action("quit")<CR>', opt)
		vim.keymap.set("n", "N", '<cmd>call ddu#ui#do_action("itemAction", {"name": "newFile"})<cr>', opt)
		vim.keymap.set("n", "d", '<cmd>call ddu#ui#do_action("itemAction", {"name": "delete"})<cr>', opt)
		vim.keymap.set("n", "r", '<cmd>call ddu#ui#do_action("itemAction", {"name": "rename"})<cr>', opt)
		vim.keymap.set("n", "y", '<cmd>call ddu#ui#do_action("itemAction", {"name": "yank"})<cr>', opt)
		vim.keymap.set("n", "c", '<cmd>call ddu#ui#do_action("itemAction", {"name": "copy"})<cr>', opt)
		vim.keymap.set("n", "p", '<cmd>call ddu#ui#do_action("itemAction", {"name": "paste"})<cr>', opt)
	end,
})

-- opt = { noremap = true, silent = true }
vim.keymap.set("n", "<Leader>sf", '<cmd>call ddu#start({"name":"filer"})<cr>', { noremap = true, silent = true })
