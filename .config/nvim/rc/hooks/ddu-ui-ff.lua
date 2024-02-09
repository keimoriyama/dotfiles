-- lua_add {{{
local path = vim.fn.expand("%:p")
if vim.fn.isdirectory(path) == 1 then
	vim.api.nvim_create_autocmd({ "BufEnter" }, {
		once = true,
		callback = function()
			-- Dduの設定を読み込む
			vim.cmd("Ddu file -name=filer")
		end,
	})
	return
end

-- }}}
-- lua_source{{{
vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-ff",
	callback = function()
		local opt = { buffer = true, silent = true }
		vim.keymap.set("n", "<CR>", '<cmd>call ddu#ui#do_action("itemAction", {"name": "open"})<CR>', opt)
		vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#do_action("toggleSelectItem")<CR>', opt)
		vim.keymap.set("n", "i", '<cmd>call ddu#ui#do_action("openFilterWindow")<CR>', opt)
		vim.keymap.set("n", "q", '<cmd>call ddu#ui#do_action("quit")<CR>', opt)
		vim.keymap.set("n", "<C-p>", '<cmd>call ddu#ui#do_action("togglePreview")<CR>', opt)
		vim.keymap.set("n", "<C-c>", '<cmd>call ddu#ui#do_action("closePreviewWindow")<CR>', opt)
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-ff-filter",
	callback = function()
		local opt = { buffer = true, silent = true }
		vim.keymap.set("i", "<CR>", "<esc><cmd>close<CR>", opt)
		vim.keymap.set("n", "<CR>", "<cmd>close<CR>", opt)
		vim.keymap.set("n", "q", '<cmd>close<CR><cmd>call ddu#ui#do_action("quit")<CR>', opt)
		vim.keymap.set("i", "<C-j>", '<cmd>call ddu#ui#do_action("cursorNext")', opt)
		vim.keymap.set("i", "<C-p>", '<cmd>call ddu#ui#do_action("cursorPrevious")', opt)
	end,
})

local function resize()
	local lines = vim.api.nvim_get_option("lines")
	local columns = vim.api.nvim_get_option("columns")
	local winCol = math.floor(columns / 8)
	local winRow = math.floor(lines / 2) - 2
	local winWidth = math.floor(columns - (columns / 4))
	local winHeight = math.floor(lines - (lines / 4))
	vim.fn["ddu#custom#patch_global"]({
		uiParams = {
			ff = {
				winCol = winCol,
				winRow = winRow,
				winWidth = winWidth,
				winHeight = math.floor(winHeight / 2),
				previewCol = winCol,
				previewRow = 0,
				previewWidth = winWidth,
				previewHeight = math.floor(winHeight / 2),
			},
		},
	})
end

resize()
vim.api.nvim_create_autocmd("VimResized", { callback = resize })
-- }}}
