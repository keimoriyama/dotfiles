---@type LazySpec
local spec = {
	"uga-rosa/ugaterm.nvim",
	config = function()
		local option = { noremap = true, silent = true }
		vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>UgatermOpen -toggle<cr>", option)
	end,
}

return spec
