---@type LazySpec
local spec = {
	{
		'vim-denops/denops.vim',
		config = function()
			vim.opt.rtp:prepend("./denops")
			vim.cmd([[
				 let g:denops#debug = 1
			 ]])
		end
	}
}

return spec
