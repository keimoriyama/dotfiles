---@type LazySpec
local spec = {
	{
		'vim-denops/denops.vim',
		init = function()
			vim.cmd([[
			let g:denops#debug = 0
			]])
		end,
	},
}

return spec
