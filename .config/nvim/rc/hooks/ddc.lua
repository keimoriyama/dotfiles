-- lua_add {{{
vim.fn["pum#set_option"]({
	item_orders = { "abbr", "space", "kind", "space", "menu" },
	highlight_selected = "CursorLine",
	horizontal_menu = false,
	offset_cmdcol = 0,
	padding = false,
	use_setline = false,
	max_height = 10
})
vim.keymap.set({ "i", "c" }, "<C-n>", "<cmd>call pum#map#insert_relative(+1, 'loop')<cr>")
vim.keymap.set({ "i", "c" }, "<C-p>", "<cmd>call pum#map#insert_relative(-1, 'loop')<cr>")
vim.keymap.set({ "i", "c" }, "<C-y>", "<cmd>call pum#map#confirm()<cr>")
vim.keymap.set({ "i", "c" }, "<C-e>", "<cmd>call pum#map#cancel()<cr>")
vim.fn["ddc#set_static_import_path"]()
vim.fn['ddc#custom#load_config'](vim.fn.expand("$HOME/.config/nvim/rc/ddc.ts"))
require("ddc_source_lsp_setup").setup()
require("lspconfig").denols.setup({})
-- }}}

-- lua_source {{{
-- Use ddc
function _G.CommandlinePre(mode)
	vim.b["prev_buffer_config"] = vim.fn["ddc#custom#get_buffer"]()
	if mode == ":" then
		vim.fn["ddc#custom#patch_buffer"]("sourceOptions",
			{ _ = { keywordPattern = "[0-9a-zA-Z_:#-]*", minAutoCompleteLength = 2, } })
	end
	vim.api.nvim_create_autocmd("User", {
		pattern = "DDCCmdlineLeave",
		once = true,
		callback = function()
			if vim.fn.exists("b:prev_buffer_config") then
				vim.fn["ddc#custom#set_buffer"](vim.b["prev_buffer_config"])
				vim.b["prev_buffer_config"] = nil
			end
		end
	})
	vim.fn["ddc#enable_cmdline_completion"]()
end

-- コマンドライン補完の設定
vim.keymap.set({ "n", 'x' }, ":", "<Cmd>call v:lua.CommandlinePre(':')<CR>:")
vim.fn["ddc#enable"]()
require("ddc_previewer_floating").enable()
-- }}}
