-- lua_add {{{
local opt = { noremap = true, silent = true }
-- ff
vim.keymap.set("n", "<Leader>ff", "<cmd>Ddu file_rec -ui-param-f-startFilter=v:true<cr>", opt)
-- helper
vim.keymap.set("n", "<leader>h", "<cmd>Ddu help -ui-param-f-startFilter=v:true<cr>", opt)
local path = vim.fn.getcwd()
-- rgの設定
vim.keymap.set('n', '<leader>fr', function()
	vim.fn["ddu#start"](
		{
			sources = {
				{
					name = 'rg',
					options = {
						matchers = {},
						volatile = true,
						path = path,
					},
				},
			},
			uiParams = {
				ff = {
					ignoreEmpty = false
				}
			}
		}
	)
end, opt)
vim.keymap.set("n", "<leader>sr", "<cmd>Ddu lsp_references -sync=true<cr>", opt)
vim.keymap.set("n", "<leader>ds", "<cmd>Ddu lsp_documentSymbol -sync=true<cr>", opt)
vim.keymap.set("n", "<leader>ic", "<cmd>Ddu lsp_callHierarchy -sync=true -source-params=outGoingCalls<cr>", opt)
vim.keymap.set("n", "<leader>oc", "<cmd>Ddu lsp_callHierarchy -sync=true -source-params=outgoingCalls<cr>", opt)

vim.keymap.set("n", "<Leader>sf", '<cmd>Ddu file -name=filer<cr>', opt)
vim.keymap.set("n", "<Leader>sb", '<cmd>Ddu buffer <cr>', opt)
vim.keymap.set("n", "/", '<cmd>Ddu -name=search line -ui-param-ff-startFilter=v:true<cr>', opt)
vim.keymap.set("n", "*",
	"<cmd>Ddu -name=search line -resume=v:false -input=`expand('<cword>')` -ui-param-ff-startFilter=v:false<cr>", opt)
vim.keymap.set("n", "<leader>k", "<cmd>Ddu keymaps<cr>", opt)
vim.keymap.set("n", "n", "<cmd>Ddu -resume=v:true<cr>", opt)

local function resize()
	local lines = vim.opt.lines:get()
	local height, row = math.floor(lines * 0.7), math.floor(lines * 0.05)
	local columns = vim.opt.columns:get()
	local width, col = math.floor(columns * 0.8), math.floor(columns * 0.1)
	vim.fn["ddu#custom#patch_global"]({
		uiParams = {
			ff = {
				winCol = col,
				winRow = row,
				winWidth = width,
				winHeight = height,
				previewCol = col,
				previewRow = row + 20,
				previewWidth = math.floor(width),
				previewHeight = height,
			},
		},
	})
end

vim.api.nvim_create_autocmd("VimResized", { callback = resize })
-- キーマッピングの設定
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
	end
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

resize()

vim.api.nvim_create_autocmd("FileType", {
	pattern = "ddu-filer",
	callback = function()
		local opt = { buffer = true, silent = true }
		vim.keymap.set(
			"n",
			"<CR>",
			'<cmd>call ddu#ui#do_action("itemAction", {"name":"open", "params":{"command":"tabnew"}})<CR>',
			opt
		)
		vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#do_action("toggleSelectItem")<CR>', opt)
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
vim.fn['ddu#set_static_import_path']()
vim.fn["ddu#custom#load_config"](vim.fn.expand("~/.config/nvim/rc/ddu.ts"))

local path = vim.fn.expand('%:p')
if (vim.fn.isdirectory(path) == 1) then
	vim.api.nvim_create_autocmd({ "BufEnter" },
		{
			once = true,
			callback = function()
				-- Dduの設定を読み込む
				vim.cmd("Ddu file -name=filer")
			end
		})
	return
end
-- }}}
