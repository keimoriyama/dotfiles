-- lua_source{{{
local width = vim.fn.winwidth(0)
if width >= 100 then
	require("no-neck-pain").enable()
end
vim.api.nvim_create_autocmd("VimResized", {
	callback = function()
		width = vim.fn.winwidth(0)
		if width >= 100 then
			require("no-neck-pain").enable()
		else
			require("no-neck-pain").disable()
		end
	end,
})
-- }}}
