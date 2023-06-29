vim.fn["ddu#custom#patch_global"]({
	ui = "ff",
	sources = {
		{
			name = "file_rec",
			params = {
				ignoredDirectories = { ".git", "node_modules", "vendor" },
			},
		},
	},
	sourceOptions = {
		_ = {
			matchers = { "matcher_substring" },
		},
		file_rec = {
			matchers = { "matcher_substring" },
		},
	},
	filterParams = {
		matcher_substring = {
			highlightMatched = "Title",
		},
	},
	kindOptions = {
		file = {
			defaultAction = "open",
		},
	},
	uiParams = {
		ff = {
			autoAction = { "name", "preview" },
			split = "floating",
			prompt = ">",
			startFilter = [[v:true]],
			floatingBorder = "rounded",
			floatingTitle = "Ddu ff",
			floatingTitlePos = "center",
			previewFloating = [[v:true]],
			previewFloatingBorder = "rounded",
			previewFloatingTitle = "Ddu ff preview",
			previewFloatingTitlePos = "center",
			winCol = vim.fn.wincol(),
			winRow = vim.fn.winrow(),
			--'winWidth': s:winWidth / 2,
			-- \		'winHeight': s:winHeight,
			-- \		'previewCol': s:winCol + s:winWidth / 2 + 2,
			-- \		'previewRow': s:winRow + s:winHeight + 5,
			-- \		'previewWidth': s:winWidth / 2,
			-- \		'previewHeight': s:winHeight + 3,
		},
	},
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-ff",
	callback = function()
		local opt = { buffer = true, silent = true }
		vim.keymap.set("n", "<CR>", '<cmd>call ddu#ui#do_action("itemAction", {"name": "open"})<CR>', opt)
		vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#do_action("toggleSelectItem")<CR>', opt)
		vim.keymap.set("n", "i", '<cmd>call ddu#ui#do_action("openFilterWindow")<CR>', opt)
		vim.keymap.set("n", "q", '<cmd>call ddu#ui#do_action("quit")<CR>', opt)
		vim.keymap.set("n", "p", '<cmd>call ddu#ui#do_action("preview")<CR>', opt)
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-ff-filter",
	callback = function()
		local opt = { buffer = true, silent = true }
		vim.keymap.set("i", "<CR>", "<esc><cmd>close<CR>", opt)
		vim.keymap.set("n", "<CR>", "<cmd>close<CR>", opt)
		vim.keymap.set("q", "<CR>", "<cmd>close<CR>", opt)
		vim.keymap.set("i", "<C-j>", '<cmd>call ddu#ui#do_action("cursorNext")', opt)
		vim.keymap.set("i", "<C-p>", '<cmd>call ddu#ui#do_action("cursorPrevious")', opt)
	end,
})
