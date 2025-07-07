-- lua_source{{{
local width = vim.fn.winwidth(0)
local no_neck = require("no-neck-pain")
no_neck.setup({
	buffers = {
		scratchPad = {
			enabled = true,
			pathToFile = "~/Documents/notes/test.md",
			-- fileName = "test",
		},
		bo = {
			filetype = "md",
		},
		right = {
			enabled = false,
		},
	},
})
no_neck.enable()
-- vim.api.nvim_create_autocmd("VimResized", {
-- 	callback = function()
-- 		width = vim.fn.winwidth(0)
-- 		if width >= 100 then
-- 			no_neck.enable()
-- 		else
-- 			no_neck.disable()
-- 		end
-- 	end,
-- })
-- }}}
