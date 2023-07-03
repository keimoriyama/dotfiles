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
		help = {
			defaultAction = "open",
		},
	},
	uiParams = {
		ff = {
			startFilter = true,
			split = "floating",
			prompt = "> ",
			floatingBorder = "rounded",
			floatingTitle = "Ddu ff",
			floatingTitlePos = "center",
			highlights = {
				floating = "Normal",
				floatingBorder = "Normal",
			},
			filterFloatingPosition = "bottom",
			previewFloating = true,
			previewSplit = "vertical",
			previewFloatingBorder = "rounded",
			previewFloatingTitle = "preview",
			previewFloatingTitlePos = "center",
		},
	},
})

local function resize()
	local lines = vim.opt.lines:get()
	local height, row = math.floor(lines * 0.8), math.floor(lines * 0.05)
	local columns = vim.opt.columns:get()
	local width, col = math.floor(columns * 0.7), math.floor(columns * 0.05)
	vim.fn["ddu#custom#patch_global"]({
		uiParams = {
			ff = {
				winCol = col,
				winRow = row,
				winWidth = width,
				winHeight = height,
				previewCol = math.floor(width) + col + 10,
				previewRow = row,
				previewWidth = math.floor(width),
				previewHeight = height,
			},
		},
	})
end

vim.api.nvim_create_autocmd("VimResized", { callback = resize })

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

resize()
vim.keymap.set("n", "<Leader>ff", '<cmd>call ddu#start({"name":"ff"})<CR>', { noremap = true, silent = true })
vim.keymap.set(
	"n",
	"<leader>h",
	"<cmd>call ddu#start({'sources': [{'name':'help', 'params':{}}]})<cr>",
	{ noremap = true, silent = true }
)
