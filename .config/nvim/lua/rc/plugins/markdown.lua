---@type LazySpec
local spec = {
	{
		-- use peek
		"iamcco/markdown-preview.nvim",
		build = "cd app && npm install",
		init = function()
			local opts = { noremap = true, silent = true }
			vim.g.mkdp_filetypes = { "markdown" }
			vim.keymap.set("n", "mp", "<cmd>MarkdownPreviewToggle<CR>", opts)
		end,
		ft = { "markdown" },
	},
	{ "mattn/vim-maketable", ft = { "markdown" } },
}
return spec
