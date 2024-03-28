-- lua_source{{{
local width = vim.fn.winwidth(0)
if width >= 100 then
	require("no-neck-pain").enable()
end
vim.api.nvim_craete_autocmd("VimResized", {
	callback = function()
		local width = vim.fn.winwidth(0)
		if width >= 100 then
			require("no-neck-pain").enable()
		else
			require("no-neck-pain").disable()
		end
	end,
})
-- }}}
