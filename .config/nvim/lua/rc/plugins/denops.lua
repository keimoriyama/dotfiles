---@type LazySpec
local spec = {
	{
		'vim-denops/denops.vim',
		init = function()
			vim.cmd([[
			let g:denops#debug = 1
			]])
		end,
	},
}

return spec
