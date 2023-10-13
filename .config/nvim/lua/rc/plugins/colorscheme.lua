---@type LazySpec
local spec = {
	{ "folke/tokyonight.nvim" },
	{
		"ellisonleao/gruvbox.nvim",
		priority = 100,
		config = function()
			vim.cmd([[colorscheme gruvbox]])
		end
	},
}

return spec
